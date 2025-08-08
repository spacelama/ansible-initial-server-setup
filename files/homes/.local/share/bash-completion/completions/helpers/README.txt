Forms of debugging I've found useful:

        before=$(complete -p; set | grep -v -e ^before= -e ^after=);
        _longopt;
        after=$(complete -p; set | grep -v -e ^before= -e ^after=);
        colordiff -ub <(echo "$before") <(echo "$after");


