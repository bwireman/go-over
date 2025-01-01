Output images/demo.gif
Set FontSize 14
Set PlaybackSpeed 1.5
Set TypingSpeed 75ms
Set FontFamily 'JetBrains Mono'
Set Margin 20
Set MarginFill "#ffaff3"
Set BorderRadius 20

# Setup
Hide
Type `rm -rf .go-over/ && clear`
Enter
Show

Type `gleam run -m go_over -- -h`
Enter
Sleep 12s

Type "# check for retired, vulnerable or additionally outdated packages"
Sleep 2s
Enter
Type `gleam run -m go_over -- --outdated`
Enter
Sleep 50s