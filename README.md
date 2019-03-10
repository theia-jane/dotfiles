# dotfiles

Tyler Ware's configuration files.

## Installation
Run `make` to install them.

## Design Considerations

### Scripts over aliases or functions
Scripts are preferred over aliases or functions for a few reasons.
The most basic is that a script is always available regardless of
the current shell or who you've logged in as. Compared
to aliases or functions which have limitations around when and where 
they can be used. Scripts work flawlessly for most purposes.

There is one primary case for using aliases & functions: shell builtins.


