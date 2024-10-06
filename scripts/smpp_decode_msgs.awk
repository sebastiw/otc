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

function field_type(s){
    if(s ~ /[iI]nteger/) {
        return "integer"
    } else if(s ~ /C-Octet String/) {
        return "cstring"
    } else if(s ~ /Octet String/){
        return "string"
    } else if(s ~ /TLV/) {
        return "tlv"
    } else if(s ~ /Composite/) {
        return "composite"
    } else {
        return "unknown("s")"
    }

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

}

/^\* 4 SMPP PDU/ {
    section=1;
}
/^\* 4.8 PDU/ {
    section=0;
}
!section {
    next;
}

/\*\*\* 4[^ ]+/ {
    msg=$3;
    FS="|";
    next;
}

/^\| sequence_number/ && !start {
    start=1;
    next;
}

/^$/ && start {
    start=0;
    params=substr(params, 26, length(params)-27)
    if(params != "") {
        params=sprintf("%s\n%-24s", params, " ");
    }
    fun=sprintf("decode_msg(#{command := %s}, Bin) ->\n    AllowedParameters = [%s],\n    decode_parameters(AllowedParameters, Bin);", msg, params);
    msg="";
    params="";
    print fun;
    FS=" ";
    next;
}

start {
    field=atom(trim($2));
    size=trim($3);
    type=field_type(trim($4));
    if(type == "tlv" && size == "-") {
        size="undefined";
    } else {
        size=len(size);
    }
    params=params sprintf("%-25s{%s, %s, %s},\n", " ", field, size, type);
}

END {
}
