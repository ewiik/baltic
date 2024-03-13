# Command line calls used for exposure for tahkoluoto station wind data and rauma project

## calculate the winds necessary for the wind bins

*NOTE* this script assumes the working directory to be Desktop, which is where my VS code terminal opens by default

python ./git/baltic/dat-private/python-functions/windbin2.py ./git/baltic/dat-private/rauma/dat-mod/tahko-clean.csv -b 16

## calculate the exposure using a copy paste of windbin output

python ./git/baltic/dat-private/python-functions/allvar.py ./git/baltic/dat-private/rauma/dat-mod/depth-raster-py.tiff ./git/baltic/dat-private/rauma/dat-mod/tahko-exp.tiff -v --accu isaeus2004 --attn surface --model iswm -w 6.0515580736544 5.040484429065746 3.699744897959181 3.3602112676056386 3.8712287712287754 4.917651515151516 6.159173669467794 6.650895140664966 7.17337740384616 7.197787338660096 6.715558601782037 5.508172531214533 6.364600000000006 6.906053268765139 7.200258732212156 7.030382513661209