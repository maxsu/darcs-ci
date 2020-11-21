#compdef darcs
## Darcs completion for zsh.
##
## Originally derived from a version by
## Copyright (C) 2009  Nicolas Pouillard

local -a darcs_options darcs_non_options darcs_arguments

if (($CURRENT == 2)); then
  compadd -- $(darcs --commands)
else
  case "${words[2]}"; in
    get|clone)
      _urls
      ;;
  esac
  case "${words[$CURRENT]}"; in
    /*|./*|\~*|../*)
      _files
      ;;
    -*)
      # advanced zsh (array) parameter expansion fu:
      # - ${(f)...} means split into array elements at line endings
      #   instead of white space
      # - ${arr:#pat} drops elements matching pat from arr, whereas
      #   ${(M)arr:#pat} drops non-matching elements
      # - ${arr/pat/repl} replaces pat with repl for all elements of arr
      darcs_arguments=(${(f)"$(words[$CURRENT]=--list-options && $words 2>/dev/null)"})
      darcs_options=(${${(M)darcs_arguments:#-*}/;/:})
      _describe '' darcs_options
      ;;
    *)
      darcs_arguments=(${(f)"$(words[$CURRENT]=--list-options && $words 2>/dev/null)"})
      darcs_non_options=(${darcs_arguments:#-*})
      _multi_parts -i -S ' ' / darcs_non_options
      ;;
  esac
fi
