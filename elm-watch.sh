while true; do
    find src/ -name "*.elm" | entr -d elm make src/Main.elm --output=app/avrvds.js;
    sleep 0.5;
done
