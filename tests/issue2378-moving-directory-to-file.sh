darcs initialize 
mkdir d
darcs add d
echo sometext > d/f
darcs add d/f
darcs record -am'added d/f' --skip-long-comment
darcs move d/f .
darcs record -am'moved d/f to .' --skip-long-comment
rmdir d
darcs record -am'removed d' --skip-long-comment
darcs move f d
darcs record -am'moved f to d' --skip-long-comment
darcs obliterate --last=3 --all
darcs whatsnew -l
