# Runs over doc/sccp.org

## Help functions ###################

function ltrim(s){ sub(/^[ \t\r\n]+/, "", s); return s };
function rtrim(s){ sub(/[ \t\r\n]+$/, "", s); return s };
function trim(s){ return ltrim(rtrim(s)); };

function short_name(s){
    if(match(s, /call/)) {
        return gensub(/call(e|in)(d|g)_.*/, "C\\2PA", "g", s)
    } else {
        return toupper(gensub(/([a-z])[^_]+_?/, "\\1", "g", s))
    }
}
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
    opars = gensub(/,\n$/, "", "g", opars)
    opars = gensub(/\n/, "\n                         ", "g", opars)
    vpars = gensub(/, $/, "", "g", vpars)
    fields = gensub(/,\n$/, "", "g", fields)
    fields = gensub(/\n/, "\n               ", "g", fields)
    print "decode_msg("msgt", Bin) ->"
    printf("    NumPointers = %d,\n", num_points+opt_points)
    print "    <<"fpars"Pointers:NumPointers/binary, Bin1/binary>> = Bin,"
    print "    ["vpars"] = separate_fields(Pointers, Bin1),"
    if(opt_points == 0) {
        print "    Optionals = #{},"
    } else {
        print "    AllowedParameters = ["opars"],"
        print "    Optionals = decode_parameters(OptBin, AllowedParameters),"
    }
    print "    Optionals#{"fields"};"
    num_points=0
    opt_points=0
    opars=""
    optionals=""
    fpars=""
    vpars=""
    fields=""
}

start == 2 {
    parameter=tolower(gensub(/[ /]/, "_", "g", trim($2)))
    if(parameter == "message_type") {
        next
    }
    iei="?SCCP_IEI_"toupper(parameter)
    type=trim($4)
    if(type == "O") {
        if(opt_points == 0) {
            vpars=vpars "OptBin"
            opt_points = 1
        }
        opars=opars "{"parameter", "len($5)"},\n"
    } else if(type == "V") {
        num_points += 1
        vpars=vpars short_name(parameter)", "
        fields=fields parameter" => decode_parameter("parameter", "short_name(parameter)"),\n"
    } else if(type == "F") {
        fpars=fpars ""short_name(parameter)":"len($5)"/binary, "
        fields=fields parameter" => decode_parameter("parameter", "short_name(parameter)"),\n"
    }
}
