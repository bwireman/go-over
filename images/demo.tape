Output images/demo.gif
Set FontSize 14
Set PlaybackSpeed 1.35
Set TypingSpeed 75ms
Set FontFamily 'JetBrains Mono'
Set Margin 12
Set MarginFill "#ffaff3"
Set BorderRadius 12

# Setup
Hide
Type `rm -rf ~/.cache/go-over/ && clear`
Enter
Show

Type `gleam run -m go_over -- -h`
Sleep 3.5s
Enter
Sleep 10s

Type clear
Enter

Type "# check for retired, vulnerable or outdated packages (and even filter their licenses)"
Enter
Sleep 5s
Type `gleam run -m go_over -- --outdated`
Enter
Wait+Screen@50s /✅ No warnings found!/
Sleep 3.5s