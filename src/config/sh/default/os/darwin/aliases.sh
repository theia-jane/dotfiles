alias osReference="grep '^ *kVK' /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h|tr -d ,|while read x y z;do printf '%d %s %s\n' $z $z ${x#kVK_};done|sort -n"
alias os.o='application=$(ls -1 /Applications | sed "s/.app//" | fzf) && open -a ${application}.app'
alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
alias c="pbcopy"
alias v="pbpaste"
