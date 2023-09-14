## Help functions ###################
function trim(s){
    sub(/^[ \t\r\n]+/, "", s)
    sub(/[ \t\r\n]+$/, "", s)
    return s
}

function atom(s){
    s=trim(tolower(s))
    gsub("[()/']", "", s)
    gsub("[ -]", "_", s)
    if(s ~ /^5.*/) {
        s = "'" s "'"
    }
    return s
}

function atom_to_var(s){
    if(substr(s, 1, 3) == "'5g") {
        s=substr(s, 4, length(s)-4)
    }
    split(s, arr, "_")
    s=""
    for(i = 1; i <= length(arr); i++) {
        l=toupper(substr(arr[i], 1, 1)) substr(arr[i], 2)
        s=s l
    }
    return s
}

function tag(s){
    sub(/-/, "", s)
    return trim(tolower(s))
}

function hex(s){
    sub(/-/, "", s)
    return trim(toupper(s))
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
BEGIN {
    FS="|"
    print "decode_ieis(<<>>) ->"
    print "    [];"
}

NR < 201 {
    next
}
NR > 338 {
    exit 0
}
$5 ~ /^[[:space:]]*$/ {
    next
}

trim($3) == "TV" {
    print "decode_ieis(<<?GTPv1C_IEI_"toupper(atom($4))":8, Value:"len($7)"/binary, Rest/binary>>) ->"
    print "    [{"atom($4)", Value}|decode_ieis(Rest)];"
}
trim($3) == "TLV" && trim($6) == "Fixed" {
    print "decode_ieis(<<?GTPv1C_IEI_"toupper(atom($4))":8, L:16, Value0:L/binary, Rest/binary>>) when L > "trim($7)" ->"
    print "    <<Value:"trim($7)"/binary>> = Value0,"
    print "    [{"atom($4)", Value}|decode_ieis(Rest)];"
    print "decode_ieis(<<?GTPv1C_IEI_"toupper(atom($4))":8, L:16, Value:L/binary, Rest/binary>>) ->"
    print "    [{"atom($4)", Value}, decode_ieis(Rest)];"
}
trim($3) == "TLV" && trim($6) != "Fixed" {
    print "decode_ieis(<<?GTPv1C_IEI_"toupper(atom($4))":8, L:16, Value:L/binary, Rest/binary>>) ->"
    print "    [{"atom($4)", Value}|decode_ieis(Rest)];"
}
END {
    print "decode_ieis(_) ->"
    print "    []."
}
