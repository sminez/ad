//! Copied from the penrose crate.
//!
//! Really this should be published as its own crate.
use std::{
    collections::vec_deque::{self, VecDeque},
    fmt,
    iter::{once, IntoIterator},
    mem::{swap, take},
};

#[macro_export]
macro_rules! stack {
    ([$($up:expr),*], $focus:expr, [$($down:expr),*]) => { $crate::ui::stack::Stack::new([$($up),*], $focus, [$($down),*]) };
    ([$($up:expr),*], $focus:expr) => { $crate::ui::stack::Stack::new([$($up),*], $focus, []) };
    ($focus:expr, [$($down:expr),*]) => { $crate::ui::stack::Stack::new([], $focus, [$($down),*]) };
    ($focus:expr, $($down:expr),+) => { $crate::ui::stack::Stack::new([], $focus, [$($down),*]) };
    ($focus:expr) => { $crate::ui::stack::Stack::new([], $focus, []) };
}

macro_rules! pop_where {
    ($self:ident, $lst:ident, $($pred:tt)+) => {{
        let placeholder = ::std::mem::take(&mut $self.$lst);
        let mut remaining = ::std::collections::VecDeque::default();
        let mut popped = None;
        let pred = $($pred)+;

        for item in placeholder.into_iter() {
            if pred(&item) {
                popped = Some(item);
            } else {
                remaining.push_back(item);
            }
        }

        ::std::mem::swap(&mut $self.$lst, &mut remaining);

        popped
    }};
}

/// A position within a [Stack].
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Position {
    /// The current focus point
    #[default]
    Focus,
    /// Above the current focus point
    Before,
    /// Below the current focus point
    After,
    /// The first element of the stack
    Head,
    /// The last element of the stack
    Tail,
}

/// A [Stack] can be thought of as a linked list with a hole punched in it to mark
/// a single element that currently holds focus (though in practice it is implemented
/// using a [VecDeque] for efficiency purposes). By convention, the main element is
/// the first element in the stack (regardless of focus). Focusing operations do not
/// reorder the elements of the stack or the resulting [Vec] that can be obtained
/// from calling [Stack::flatten].
///
/// This is a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure))
/// over a [VecDeque]. Many of the methods that mutate the structure of the Stack
/// return back a mutable reference so that they are able to be chained.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Stack<T> {
    pub(crate) up: VecDeque<T>,
    pub(crate) focus: T,
    pub(crate) down: VecDeque<T>,
}

impl<T: fmt::Display> fmt::Display for Stack<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let up: Vec<String> = self.up.iter().rev().map(|t| t.to_string()).collect();
        let down: Vec<String> = self.down.iter().map(|t| t.to_string()).collect();

        write!(
            f,
            "Stack([{}], {}, [{}])",
            up.join(", "),
            self.focus,
            down.join(", ")
        )
    }
}

impl<T> Stack<T> {
    /// Create a new Stack specifying the focused element and and elements
    /// above and below it.
    pub fn new<I, J>(up: I, focus: T, down: J) -> Self
    where
        I: IntoIterator<Item = T>,
        J: IntoIterator<Item = T>,
    {
        let mut reversed_up = VecDeque::new();
        for elem in up.into_iter() {
            reversed_up.push_front(elem);
        }

        Self {
            focus,
            up: reversed_up,
            down: down.into_iter().collect(),
        }
    }

    // NOTE: Can't implement FromIterator<T> because we disallow an empty stack
    /// For an iterator of at least one element, the first element will
    /// be focused and all remaining elements will be placed after it.
    /// For an empty iterator, return None.
    pub fn try_from_iter<I>(iter: I) -> Option<Self>
    where
        I: IntoIterator<Item = T>,
    {
        let mut it = iter.into_iter();

        let focus = match it.next() {
            Some(t) => t,
            None => return None,
        };

        Some(Self {
            up: VecDeque::default(),
            focus,
            down: it.collect(),
        })
    }

    /// The number of elements in this Stack.
    pub fn len(&self) -> usize {
        self.up.len() + self.down.len() + 1
    }

    /// Always false: a Stack always has at least one focused element.
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Provide an iterator over this stack iterating over up,
    /// focus and then down.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            up: self.up.iter(),
            focus: Some(&self.focus),
            down: self.down.iter(),
        }
    }

    /// Provide an iterator over this stack iterating over up,
    /// focus and then down with mutable references.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            up: self.up.iter_mut(),
            focus: Some(&mut self.focus),
            down: self.down.iter_mut(),
        }
    }

    /// Iterate over the clients in this stack from the the focused element
    /// down through the stack.
    pub fn unravel(&self) -> impl Iterator<Item = &T> {
        once(&self.focus)
            .chain(self.down.iter())
            .chain(self.up.iter().rev())
    }

    /// Flatten a Stack into a Vector, losing the information of which
    /// element is focused.
    pub fn flatten(self) -> Vec<T> {
        self.into_iter().map(|(_, t)| t).collect()
    }

    /// Return a reference to the first element in this [Stack]
    pub fn head(&self) -> &T {
        self.up.back().unwrap_or(&self.focus)
    }

    /// Return a reference to the focused element in this [Stack]
    pub fn focused(&self) -> &T {
        &self.focus
    }

    /// Return a reference to the last element in this [Stack]
    pub fn last(&self) -> &T {
        self.down.back().unwrap_or(&self.focus)
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.down.back_mut().unwrap_or(&mut self.focus)
    }

    /// Swap the current head element with the focused element in the
    /// stack order. Focus stays with the original focused element.
    pub fn swap_focus_and_head(&mut self) -> &mut Self {
        let mut tmp = take(&mut self.up);

        if let Some(head) = tmp.pop_back() {
            self.down.push_front(head);
        }

        for item in tmp.into_iter() {
            self.down.push_front(item);
        }

        self
    }

    /// Rotate the Stack until the current focused element is in the head position
    pub fn rotate_focus_to_head(&mut self) -> &mut Self {
        if self.up.is_empty() {
            return self;
        }

        for item in take(&mut self.up).into_iter().rev() {
            self.down.push_back(item);
        }

        self
    }

    /// Move focus to the element in the head position
    pub fn focus_head(&mut self) -> &mut Self {
        let mut head = match self.up.pop_back() {
            None => return self, // focus is already head
            Some(t) => t,
        };

        swap(&mut head, &mut self.focus);
        self.down.push_front(head);

        for item in take(&mut self.up).into_iter().rev() {
            self.down.push_front(item);
        }

        self
    }

    /// Move focus to the element in the head position
    pub fn focus_tail(&mut self) -> &mut Self {
        let mut tail = match self.down.pop_back() {
            None => return self, // focus is already tail
            Some(t) => t,
        };

        swap(&mut tail, &mut self.focus);
        self.up.push_front(tail);

        for item in take(&mut self.down).into_iter() {
            self.up.push_front(item);
        }

        self
    }

    /// Insert the given element in place of the current focus, pushing
    /// the current focus down the [Stack].
    pub fn insert(&mut self, t: T) -> &mut Self {
        self.insert_at(Position::default(), t)
    }

    /// Insert the given element at the requested position in the [Stack].
    /// See [Position] for the semantics of each case. For all cases, the
    /// existing elements in the [Stack] are pushed down to make room for
    /// the new one.
    pub fn insert_at(&mut self, pos: Position, mut t: T) -> &mut Self {
        use Position::*;

        match pos {
            Focus => {
                self.swap_focus(&mut t);
                self.down.push_front(t);
            }
            Before => self.up.push_front(t),
            After => self.down.push_front(t),
            Head => self.up.push_back(t),
            Tail => self.down.push_back(t),
        };

        self
    }

    /// Remove the focused element of this Stack. If this was the only element then
    /// the stack is dropped and None is returned.
    pub fn remove_focused(mut self) -> (T, Option<Self>) {
        let focus = match self.down.pop_front().or_else(|| self.up.pop_front()) {
            Some(focus) => focus,
            None => return (self.focus, None),
        };

        (
            self.focus,
            Some(Self {
                focus,
                up: self.up,
                down: self.down,
            }),
        )
    }

    /// Remove an element from the stack.
    ///
    /// If the element was present it is returned along with the rest of the [Stack].
    /// If this was the last element in the stack, the stack is dropped and None is
    /// returned.
    pub fn remove(mut self, t: &T) -> (Option<T>, Option<Self>)
    where
        T: PartialEq,
    {
        if let Some(found) = pop_where!(self, up, |elem: &T| elem == t) {
            return (Some(found), Some(self));
        }

        if let Some(found) = pop_where!(self, down, |elem: &T| elem == t) {
            return (Some(found), Some(self));
        }

        if t == &self.focus {
            let (focus, stack) = self.remove_focused();
            (Some(focus), stack)
        } else {
            (None, Some(self))
        }
    }

    /// Map a function over all elements in this [Stack], returning a new one.
    pub fn map<F, U>(self, f: F) -> Stack<U>
    where
        F: Fn(T) -> U,
    {
        Stack {
            focus: f(self.focus),
            up: self.up.into_iter().map(&f).collect(),
            down: self.down.into_iter().map(&f).collect(),
        }
    }

    /// Retain only elements which satisfy the given predicate. If the focused
    /// element is removed then focus shifts to the first remaining element
    /// after it, if there are no elements after then focus moves to the first
    /// remaining element before. If no elements satisfy the predicate then
    /// None is returned.
    pub fn filter<F>(self, f: F) -> Option<Self>
    where
        F: Fn(&T) -> bool,
    {
        let new_stack = Self {
            focus: self.focus,
            up: self.up.into_iter().filter(&f).collect(),
            down: self.down.into_iter().filter(&f).collect(),
        };

        if f(&new_stack.focus) {
            Some(new_stack)
        } else {
            let (_, maybe_stack) = new_stack.remove_focused();
            maybe_stack
        }
    }

    /// Reverse the ordering of a Stack (up becomes down) while maintaining
    /// focus.
    #[inline]
    pub fn reverse(&mut self) -> &mut Self {
        swap(&mut self.up, &mut self.down);

        self
    }

    #[inline]
    pub(crate) fn swap_focus(&mut self, new: &mut T) {
        swap(&mut self.focus, new);
    }

    #[inline]
    fn rev_up(&mut self) -> &mut Self {
        let mut reversed = take(&mut self.up).into_iter().rev().collect();
        swap(&mut self.up, &mut reversed);

        self
    }

    #[inline]
    fn rev_down(&mut self) -> &mut Self {
        let mut reversed = take(&mut self.down).into_iter().rev().collect();
        swap(&mut self.down, &mut reversed);

        self
    }

    /// Move focus from the current element up the stack, wrapping to the
    /// bottom if focus is already at the top.
    pub fn focus_up(&mut self) -> &mut Self {
        match (self.up.is_empty(), self.down.is_empty()) {
            // xs:x f ys   -> xs x f:ys
            // xs:x f []   -> xs x f
            (false, _) => {
                let mut focus = self.up.pop_front().expect("non-empty");
                self.swap_focus(&mut focus);
                self.down.push_front(focus);
            }

            // [] f ys:y   -> f:ys y []
            (true, false) => {
                let mut focus = self.down.pop_back().expect("non-empty");
                self.swap_focus(&mut focus);
                self.down.push_front(focus);
                self.reverse().rev_up();
            }

            // [] f []     -> [] f []
            (true, true) => (),
        }

        self
    }

    /// Move focus from the current element down the stack, wrapping to the
    /// top if focus is already at the bottom.
    pub fn focus_down(&mut self) -> &mut Self {
        match (self.up.is_empty(), self.down.is_empty()) {
            // xs f y:ys   -> xs:f y ys
            // [] f y:ys   -> f y ys
            (_, false) => {
                let mut focus = self.down.pop_front().expect("non-empty");
                self.swap_focus(&mut focus);
                self.up.push_front(focus);
            }

            // x:xs f []   -> [] x xs:f
            (false, true) => {
                let mut focus = self.up.pop_back().expect("non-empty");
                self.swap_focus(&mut focus);
                self.up.push_front(focus);
                self.reverse().rev_down();
            }

            // [] f []     -> [] f []
            (true, true) => (),
        }

        self
    }

    /// Focus the first element found matching the given predicate function.
    ///
    /// If no matching elements are found, the Stack will be left in
    /// its original state.
    pub fn focus_element_by<F>(&mut self, f: F)
    where
        F: Fn(&T) -> bool,
    {
        for _ in 0..self.len() {
            if f(&self.focus) {
                return;
            }
            self.focus_down();
        }
    }

    /// Swap the focused element with the one above, wrapping from top to bottom.
    /// The currently focused element is maintained by this operation.
    pub fn swap_up(&mut self) -> &mut Self {
        match self.up.pop_front() {
            Some(t) => {
                self.down.push_front(t);
                self
            }
            None => self.reverse().rev_up(),
        }
    }

    /// Swap the focused element with the one below, wrapping from top to bottom.
    /// The currently focused element is maintained by this operation.
    pub fn swap_down(&mut self) -> &mut Self {
        match self.down.pop_front() {
            Some(t) => {
                self.up.push_front(t);
                self
            }
            None => self.reverse().rev_down(),
        }
    }

    /// Rotate all elements of the stack forward, wrapping from top to bottom.
    /// The currently focused element in the stack is maintained by this operation.
    pub fn rotate_up(&mut self) -> &mut Self {
        match self.up.pop_back() {
            Some(t) => {
                self.down.push_back(t);
                self
            }
            None => self.reverse().rev_up(),
        }
    }

    /// Rotate all elements of the stack back, wrapping from bottom to top.
    /// The currently focused element in the stack is maintained by this operation.
    pub fn rotate_down(&mut self) -> &mut Self {
        match self.down.pop_back() {
            Some(t) => {
                self.up.push_back(t);
                self
            }
            None => self.reverse().rev_down(),
        }
    }
}

impl<T: Clone> Stack<T> {
    /// Extract elements satisfying a predicate into a Vec, leaving remaining
    /// elements in their original stack position.
    pub fn extract<F>(&self, f: F) -> (Option<Self>, Vec<T>)
    where
        F: Fn(&T) -> bool,
    {
        let mut extracted = Vec::new();
        let mut new_stack = Self {
            focus: self.focus.clone(),
            up: Default::default(),
            down: Default::default(),
        };

        for t in self.up.clone().into_iter().rev() {
            if f(&t) {
                new_stack.up.push_front(t);
            } else {
                extracted.push(t);
            }
        }

        let up_to_focus = extracted.len();

        for t in self.down.clone().into_iter() {
            if f(&t) {
                new_stack.down.push_back(t);
            } else {
                extracted.push(t);
            }
        }

        if f(&new_stack.focus) {
            return (Some(new_stack), extracted);
        }

        let (t, maybe_stack) = new_stack.remove_focused();
        extracted.insert(up_to_focus, t);

        (maybe_stack, extracted)
    }
}

impl<T: PartialEq> Stack<T> {
    /// Check whether a given element is in this Stack
    pub fn contains(&self, t: &T) -> bool {
        &self.focus == t || self.up.contains(t) || self.down.contains(t)
    }

    /// Attempt to focus a given element in the [Stack] if it is present.
    ///
    /// If the requested element is not found, the Stack will be left in
    /// its original state.
    pub fn focus_element(&mut self, t: &T) {
        self.focus_element_by(|elem| elem == t)
    }
}

// Iteration

/// An owned iterator over a [Stack].
#[derive(Debug)]
pub struct IntoIter<T> {
    up: VecDeque<T>,
    focus: Option<T>,
    down: VecDeque<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = (bool, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.up
            .pop_back()
            .map(|t| (false, t))
            .or_else(|| self.focus.take().map(|t| (true, t)))
            .or_else(|| self.down.pop_front().map(|t| (false, t)))
    }
}

impl<T> IntoIterator for Stack<T> {
    type Item = (bool, T);
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> IntoIter<T> {
        IntoIter {
            up: self.up,
            focus: Some(self.focus),
            down: self.down,
        }
    }
}

/// An iterator over a [Stack].
#[derive(Debug)]
pub struct Iter<'a, T> {
    up: vec_deque::Iter<'a, T>,
    focus: Option<&'a T>,
    down: vec_deque::Iter<'a, T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (bool, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.up
            .next_back()
            .map(|t| (false, t))
            .or_else(|| self.focus.take().map(|t| (true, t)))
            .or_else(|| self.down.next().map(|t| (false, t)))
    }
}

impl<'a, T> IntoIterator for &'a Stack<T> {
    type Item = (bool, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

/// A mutable iterator over a [Stack].
#[derive(Debug)]
pub struct IterMut<'a, T> {
    up: vec_deque::IterMut<'a, T>,
    focus: Option<&'a mut T>,
    down: vec_deque::IterMut<'a, T>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = (bool, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        self.up
            .next_back()
            .map(|t| (false, t))
            .or_else(|| self.focus.take().map(|t| (true, t)))
            .or_else(|| self.down.next().map(|t| (false, t)))
    }
}

impl<'a, T> IntoIterator for &'a mut Stack<T> {
    type Item = (bool, &'a mut T);
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test]
    fn focused() {
        let s = stack!([1, 2], 3, [4, 5]);

        assert_eq!(s.focused(), &3)
    }

    #[test]
    fn head() {
        let s = stack!([1, 2], 3, [4, 5]);

        assert_eq!(s.head(), &1)
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!(3, [2, 1, 4, 5]); "items up and down")]
    #[test_case(stack!([1, 2], 3), stack!(3, [2, 1]); "items up")]
    #[test_case(stack!(3, [4, 5]), stack!(3, [4, 5]); "items down")]
    #[test_case(stack!(3), stack!(3); "focus only")]
    #[test]
    fn swap_focus_and_head(mut s: Stack<u8>, expected: Stack<u8>) {
        s.swap_focus_and_head();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!(3, [4, 5, 1, 2]); "items up and down")]
    #[test_case(stack!([1, 2], 3), stack!(3, [1, 2]); "items up")]
    #[test_case(stack!(3, [4, 5]), stack!(3, [4, 5]); "items down")]
    #[test_case(stack!(3), stack!(3); "focus only")]
    #[test]
    fn rotate_focus_to_head(mut s: Stack<u8>, expected: Stack<u8>) {
        s.rotate_focus_to_head();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!(1, [2, 3, 4, 5]); "items up and down")]
    #[test_case(stack!([1, 2], 3), stack!(1, [2, 3]); "items up")]
    #[test_case(stack!(3, [4, 5]), stack!(3, [4, 5]); "items down")]
    #[test_case(stack!(3), stack!(3); "focus only")]
    #[test]
    fn focus_head(mut s: Stack<u8>, expected: Stack<u8>) {
        s.focus_head();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([1, 2, 3, 4], 5); "items up and down")]
    #[test_case(stack!([1, 2], 3), stack!([1, 2], 3); "items up")]
    #[test_case(stack!(3, [4, 5]), stack!([3, 4], 5); "items down")]
    #[test_case(stack!(3), stack!(3); "focus only")]
    #[test]
    fn focus_tail(mut s: Stack<u8>, expected: Stack<u8>) {
        s.focus_tail();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5, 6]), |&e| e == 3, stack!([1, 2], 3, [4, 5, 6]); "current focus")]
    #[test_case(stack!([1, 2], 3, [4, 5, 6]), |&e| e > 4, stack!([1, 2, 3, 4], 5, [6]); "in tail")]
    #[test_case(stack!([1, 2], 3, [4, 5, 6]), |&e| e < 3 && e > 1, stack!([1], 2, [3, 4, 5, 6]); "in head")]
    #[test_case(stack!([1, 2], 3, [4, 5, 6]), |&e| e < 3, stack!([], 1, [2, 3, 4, 5, 6]); "in head multiple matches")]
    #[test_case(stack!([1, 2], 3, [4, 5, 6]), |&e| e == 42, stack!([1, 2], 3, [4, 5, 6]); "not found")]
    #[test_case(stack!([1, 2], 3, [4, 5, 3, 6]), |&e| e == 42, stack!([1, 2], 3, [4, 5, 3, 6]); "not found with current focus duplicated")]
    #[test]
    fn focus_element_by(mut s: Stack<u8>, predicate: fn(&u8) -> bool, expected: Stack<u8>) {
        s.focus_element_by(predicate);

        assert_eq!(s, expected);
    }

    #[test]
    fn iter_yields_all_elements_in_order() {
        let s = stack!([1, 2], 3, [4, 5]);
        let elems: Vec<(bool, u8)> = s.iter().map(|(b, t)| (b, *t)).collect();

        assert_eq!(
            elems,
            vec![(false, 1), (false, 2), (true, 3), (false, 4), (false, 5)]
        )
    }

    #[test]
    fn iter_mut_yields_all_elements_in_order() {
        let mut s = stack!([1, 2], 3, [4, 5]);
        let elems: Vec<(bool, u8)> = s.iter_mut().map(|(b, t)| (b, *t)).collect();

        assert_eq!(
            elems,
            vec![(false, 1), (false, 2), (true, 3), (false, 4), (false, 5)]
        )
    }

    #[test]
    fn into_iter_yields_all_elements_in_order() {
        let s = stack!([1, 2], 3, [4, 5]);
        let elems: Vec<(bool, u8)> = s.into_iter().collect();

        assert_eq!(
            elems,
            vec![(false, 1), (false, 2), (true, 3), (false, 4), (false, 5)]
        )
    }

    #[test]
    fn map_preserves_structure() {
        let s = stack!(["a", "bunch"], "of", ["string", "refs"]);

        let mapped = s.map(|x| x.len());
        let expected = stack!([1, 5], 2, [6, 4]);

        assert_eq!(mapped, expected);
    }

    #[test_case(|&x| x > 5, None; "returns None if no elements satisfy the predicate")]
    #[test_case(|x| x % 2 == 1, Some(stack!([3], 1, [5])); "holds focus with predicate")]
    #[test_case(|x| x % 2 == 0, Some(stack!([2], 4)); "moves focus to top of down when possible")]
    #[test_case(|&x| x == 2 || x == 3, Some(stack!([2], 3)); "moves focus to end of up if down is empty")]
    #[test]
    fn filter(predicate: fn(&usize) -> bool, expected: Option<Stack<usize>>) {
        let filtered = stack!([2, 3], 1, [4, 5]).filter(predicate);

        assert_eq!(filtered, expected);
    }

    #[test_case(|&x| x > 5, None, vec![2,3,1,4,5]; "no elements satisfy the predicate")]
    #[test_case(|x| x % 2 == 1, Some(stack!([3], 1, [5])), vec![2,4]; "holds focus with predicate")]
    #[test_case(|x| x % 2 == 0, Some(stack!([2], 4)), vec![3,1,5]; "moves focus to top of down when possible")]
    #[test_case(|&x| x == 2 || x == 3, Some(stack!([2], 3)), vec![1,4,5]; "moves focus to end of up if down is empty")]
    #[test]
    fn extract(
        predicate: fn(&usize) -> bool,
        expected: Option<Stack<usize>>,
        expected_extracted: Vec<usize>,
    ) {
        let (s, extracted) = stack!([2, 3], 1, [4, 5]).extract(predicate);

        assert_eq!(s, expected);
        assert_eq!(extracted, expected_extracted);
    }

    #[test]
    fn flatten_is_correctly_ordered() {
        let res = stack!([1, 2], 3, [4, 5]).flatten();

        assert_eq!(res, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn try_from_iter_is_correctly_ordered() {
        let res = Stack::try_from_iter(vec![1, 2, 3, 4, 5]);

        assert_eq!(res, Some(stack!(1, [2, 3, 4, 5])));
    }

    #[test]
    fn try_from_iter_of_empty_iterable_is_none() {
        let empty: Vec<()> = vec![];

        assert_eq!(Stack::try_from_iter(empty), None);
    }

    #[test]
    fn try_from_iter_after_flatten_with_empty_up_is_inverse() {
        let s = stack!(1, [2, 3, 4]);
        let res = Stack::try_from_iter(s.clone().flatten());

        assert_eq!(res, Some(s));
    }

    #[test]
    fn reverse_holds_focus() {
        let mut s = stack!([1, 2], 3, [4, 5]);
        s.reverse();

        assert_eq!(s, stack!([5, 4], 3, [2, 1]));
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([1], 2, [3, 4, 5]); "items up and down")]
    #[test_case(stack!([], 1, [2, 3]), stack!([1, 2], 3); "items down only")]
    #[test_case(stack!([1, 2], 3, []), stack!([1], 2, [3]); "items up only")]
    #[test_case(stack!([], 1, []), stack!(1); "only focused")]
    #[test]
    fn focus_up(mut s: Stack<usize>, expected: Stack<usize>) {
        s.focus_up();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([1, 2, 3], 4, [5]); "items up and down")]
    #[test_case(stack!(1, [2, 3]), stack!([1], 2, [3]); "items down only")]
    #[test_case(stack!([1, 2], 3), stack!(1, [2, 3]); "items up only")]
    #[test_case(stack!(1), stack!(1); "only focused")]
    #[test]
    fn focus_down(mut s: Stack<usize>, expected: Stack<usize>) {
        s.focus_down();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([1], 3, [2, 4, 5]); "items up and down")]
    #[test_case(stack!(1, [2, 3]), stack!([2, 3], 1); "items down only")]
    #[test_case(stack!([1, 2], 3), stack!([1], 3, [2]); "items up only")]
    #[test_case(stack!(1), stack!(1); "only focused")]
    #[test]
    fn swap_up(mut s: Stack<usize>, expected: Stack<usize>) {
        s.swap_up();

        assert_eq!(s, expected);
    }

    #[test]
    fn swap_up_chained() {
        let mut s = stack!([1, 2], 3, [4]);

        s.swap_up();
        assert_eq!(s, stack!([1], 3, [2, 4]));
        s.swap_up();
        assert_eq!(s, stack!(3, [1, 2, 4]));
        s.swap_up();
        assert_eq!(s, stack!([1, 2, 4], 3));
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([1, 2, 4], 3, [5]); "items up and down")]
    #[test_case(stack!(1, [2, 3]), stack!([2], 1, [3]); "items down only")]
    #[test_case(stack!([1, 2], 3), stack!(3, [1, 2]); "items up only")]
    #[test_case(stack!(1), stack!(1); "only focused")]
    #[test]
    fn swap_down(mut s: Stack<usize>, expected: Stack<usize>) {
        s.swap_down();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([2], 3, [4, 5, 1]); "items up and down")]
    #[test_case(stack!(1, [2, 3]), stack!([2, 3], 1); "items down only")]
    #[test_case(stack!([1, 2], 3), stack!([2], 3, [1]); "items up only")]
    #[test_case(stack!(1), stack!(1); "only focused")]
    #[test]
    fn rotate_up(mut s: Stack<usize>, expected: Stack<usize>) {
        s.rotate_up();

        assert_eq!(s, expected);
    }

    #[test_case(stack!([1, 2], 3, [4, 5]), stack!([5, 1, 2], 3, [4]); "items up and down")]
    #[test_case(stack!(1, [2, 3]), stack!([3], 1, [2]); "items down only")]
    #[test_case(stack!([1, 2], 3), stack!(3, [1, 2]); "items up only")]
    #[test_case(stack!(1), stack!(1); "only focused")]
    #[test]
    fn rotate_down(mut s: Stack<usize>, expected: Stack<usize>) {
        s.rotate_down();

        assert_eq!(s, expected);
    }

    #[test_case(Position::Focus, stack!([1,2], 6, [3,4,5]); "focus")]
    #[test_case(Position::Before, stack!([1,2,6], 3, [4,5]); "before")]
    #[test_case(Position::After, stack!([1,2], 3, [6,4,5]); "after")]
    #[test_case(Position::Head, stack!([6,1,2], 3, [4,5]); "head")]
    #[test_case(Position::Tail, stack!([1,2], 3, [4,5,6]); "tail")]
    #[test]
    fn insert_at(pos: Position, expected: Stack<usize>) {
        let mut s = stack!([1, 2], 3, [4, 5]);
        s.insert_at(pos, 6);

        assert_eq!(s, expected);
    }
}
