Output images/demo.gif
Set FontSize 14
Set PlaybackSpeed 1.35
Set TypingSpeed 75ms
Set FontFamily 'JetBrains Mono'
Set Margin 15
Set MarginFill "#ffaff3"
Set BorderRadius 12

# Setup
Hide
Type `rm -rf .go-over/`
Enter
Type `rm -rf ~/.go-over/ && clear`
Enter
Show

Type `gleam run -m go_over -- -h`
Sleep 2s
Enter
Sleep 10s

Type clear
Enter

Type "# check for retired, vulnerable (& optionally outdated) packages"
Enter
Sleep 5s
Type `gleam run -m go_over -- --outdated`
Enter
Sleep 40s