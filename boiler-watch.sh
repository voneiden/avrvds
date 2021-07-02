while true; do
    find src/ -name "*.elm" | entr -d python ../elm_type_boiler/elm_type_boiler.py;
    sleep 0.5;
done
