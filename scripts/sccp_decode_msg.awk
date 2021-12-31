# Runs over doc/sccp.org

## Help functions ###################

function ltrim(s){ sub(/^[ \t\r\n]+/, "", s); return s };
function rtrim(s){ sub(/[ \t\r\n]+$/, "", s); return s };
function trim(s){ return ltrim(rtrim(s)); };
function len(s){
    if(index(s, "-")){
        split(s, arr, "-")
        return sprintf("{%s, %s}", trim(arr[1]), trim(tolower(arr[2])))
    } else if(trim(s) == "1/2") {
        return "half"
    } else {
        return trim(s)
    }
}


## Execution ###################

BEGIN { FS="|" }

/^** 4\./ {
    msgt=tolower(gensub(/^.*\((.*)\)$/, "\\1", 1))
    start=1
    next
}
/*/ {
    start=0
    next
}
start && /---/ {
    start=2
    next
}
start == 2 && /^$/ {
    start=1
    print "decode_msg("msgt", Bin) ->"
    pars = gensub(/,\n$/, "]", "g", pars)
    pars = gensub(/\n/, "\n                         ", "g", pars)
    print "    AllowedParameters = ["pars","
    print "    decode_parameters(Bin, AllowedParameters);"
    pars=""
}

start == 2 {
    parameter=tolower(gensub(/ /, "_", "g", trim($2)))
    if(parameter == "message_type") {
        next
    }
    iei="?SCCP_IEI_"toupper(parameter)
    type=gensub(/V/, "variable", "g", gensub(/O/, "optional", "g", gensub(/F/, "fixed", "g", trim($4))))
    pars=pars "{"parameter", "type", "len($5)"},\n"
}
