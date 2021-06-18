while true; do
    ls -d src/*.elm | entr -d elm make src/Main.elm --output=app/avrvds.js;
    sleep 0.5;
done
