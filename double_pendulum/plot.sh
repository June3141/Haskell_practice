stack ghc Main.hs
./Main
python plot.py
ffmpeg -r 60 -i ./picture/%05d.png -pix_fmt yuv420p -r 60 simulation.mp4 -y
rm -f ./picture/*.png
