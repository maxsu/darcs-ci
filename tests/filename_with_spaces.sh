. lib

darcs init R
cd R

filename="file with spaces in its name"
echo xxx > "$filename"
darcs record -lam "comment"
darcs log -s | grep -c "$filename" | grep -w 1
darcs log -v | grep -c "$filename" | grep -w 2
