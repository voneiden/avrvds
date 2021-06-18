tmux \
    new-session  'sh elm-watch.sh' \; \
    split-window 'sh sass-watch.sh' \; \
    split-window 'five-server ./app/' \;
#detach-client

