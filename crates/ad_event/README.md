# ad_event :: a shared event format for ad and client programs

### Example Format
```
M L 160 173 14 | screenshot.png
```

```bash
#!/usr/bin/env sh

9p read ad/buffers/1/event | while read -r line; do
    echo "got event: $line"
    txt=$(echo "$line" | sed 's/.*| //')
    if [ "$txt" = "README.md" ]; then
        echo "  > allowing event"
        echo "$line" | 9p write ad/buffers/1/event
    else
        echo "  > suppressing event"
    fi
done
```
