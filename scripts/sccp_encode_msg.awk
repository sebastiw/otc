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
function atom_to_var(s){
    split(s, arr, "_")
    s=""
    for(i = 1; i <= length(arr); i++) {
        l=toupper(substr(arr[i], 1, 1)) substr(arr[i], 2)
        s=s l
    }
    return s
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
    fpars = gensub(/,\n$/, ",", "g", fpars)
    fpars = gensub(/\n/, "\n    ", "g", fpars)
    vbin = gensub(/, $/, ",\n      ", "g", vbin)
    fields = gensub(/,\n$/, "", "g", fields)
    fields = gensub(/\n/, "\n             ", "g", fields)
    pointers=""
    for(i = 0; i < num_points; i++) {
        if(i == 0) {
            vlength = num_points+opt_points
        } else {
            vlength = sprintf("%s+%s", vlen[i], vlength)
        }
        pointers=pointers sprintf("(%s):8/big, ", vlength)
    }
    pointers = gensub(/, $/, "", "g", pointers)
    print "encode_msg("msgt","
    print "           #{"fields"} = Msg) ->"
    print "    "fpars
    print "    AllowedParameters = ["opars"],"
    print "    OptBin = encode_parameters(Msg, AllowedParameters),"
    if(opars == "") {
        print "    Pointers = <<"pointers">>,"
    } else {
        if (vlen[i] == "") {
            print "    OptPointer = case byte_size(OptBin) > 0 of"
            print "                     true ->"
            print "                         1;"
            print "                     false ->"
            print "                         0"
            print "                 end,"
            print "    Pointers = <<OptPointer:8/big>>,"
        } else {
            print "    OptPointer = case byte_size(OptBin) > 0 of"
            print "                     true ->"
            printf("                         %s+%s;\n", vlen[i], vlength)
            print "                     false ->"
            print "                         0"
            print "                 end,"
            print "    Pointers = <<"pointers", OptPointer:8/big>>,"
        }
    }
    print "    <<"fbin"Pointers/binary,\n      "vbin"OptBin/binary>>;"
    var_points=var_points "<<>>,"
    num_points=0
    opt_points=0
    opars=""
    fpars=""
    fbin=""
    vbin=""
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
        opt_points = 1
        opars=opars "{"parameter", "len($5)"},\n"
    } else if(type == "V") {
        num_points += 1
        fpars=fpars ""short_name(parameter)" = encode_parameter("parameter", "atom_to_var(parameter)"),\n"
        fpars=fpars ""short_name(parameter)"Len = byte_size("short_name(parameter)"),\n"
        vlen[num_points]=short_name(parameter)"Len"
        vbin=vbin short_name(parameter)"Len:8/big, "short_name(parameter)"/binary, "
        fields=fields parameter" := "atom_to_var(parameter)",\n"
    } else if(type == "F") {
        fpars=fpars ""short_name(parameter)" = encode_parameter("parameter", "atom_to_var(parameter)"),\n"
        fbin=fbin short_name(parameter)"/binary, "
        fields=fields parameter" := "atom_to_var(parameter)",\n"
    }
}
