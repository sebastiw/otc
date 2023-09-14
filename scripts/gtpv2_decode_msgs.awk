## Help functions ###################
function trim(s){
    sub(/^[ \t\r\n]+/, "", s)
    sub(/[ \t\r\n]+$/, "", s)
    return s
}

function atom(s){
    s=trim(tolower(s))
    gsub("[()/',]", "", s)
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

/^\* 6/ {
    in_msgs=1
    next;
}
/\|--/ && in_msgs {
    active=1
    FS="|"
    next;
}
/^$/ && active && in_msgs {
    FS=" "
    active=0
    in_msgs=0
    print
    next;
}

$3 !~ /(For future use|Reserved)/ && active && in_msgs {
    msg_type=atom(trim($3))
    msg_code=trim($2)
    print "-define(GTPv2_MSG_TYPE_"toupper(msg_type)", "msg_code")."
    next;
}

# Type of message
/^\*\* 7[^ ]+/ {
    inside_messages=1
    msg_type=atom(substr($0, length($1)+length($2)+3))
    msg_type=gensub("_messages$", "", 1, msg_type)
}
# !inside_messages { next; }

/^\* 8/ {
    inside_iei=1
}
# Find the separator line in table
/\|--/ && inside_iei {
    active=1
    FS="|"
    next
}
/^$/ && active && inside_iei {
    inside_iei=0
    active=0
    print
    FS=" "
}
$3 !~ /.*(Spare|Reserved).*/ && active && inside_iei {
    iei_ref=trim($4)
    iei_name=atom(trim($3))
    iei_code=trim($2)
    iei_type="TLIV"
    iei_length_type=trim($5)
    iei_length=trim($6)

    iei_names[iei_ref]=iei_name
    iei_codes[iei_ref]=iei_code
    iei_types[iei_ref]=iei_type
    iei_lengths[iei_ref]=len(iei_length)
    iei_length_types[iei_ref]=iei_length_type
    printf("-define(GTPv2_IEI_%s, %s).\n", toupper(iei_name), iei_code)
}
active && !inside_messages {
    next;
}

# Every chapter (message)
/\*\*\*/ && inside_messages {
    header=tolower(substr($0, length($1)+length($2)+3))
    gsub(" ", "_", header)
    gsub("[-()]", "", header)
}

# Find the separator line in table
/\|--/ && inside_messages {
    active=1
    FS="|"
    printf("decode_msg(%s, Bin0) ->\n", header)
    fields=sprintf("               message_group => %s,\n", msg_type)
    next
}

# Empty lines in bottom of table finishes execution
/^$/ && active {
    FS=" "
    active=0
    printf("%s", parse_man_s)
    # Remove trailing comma and prefix whitespace
    fields=trim(fields)
    fields=substr(fields, 1, length(fields)-1)
    variables=trim(variables)
    variables=substr(variables, 1, length(variables)-1)
    printf("    Fields = [%s],\n", variables)
    printf("    {Msg, _Unknown} = decode_tliv_list(Bin%d, Fields),\n", bin_ctr)
    if(fields == "") {
        printf("    Msg;\n")
    } else {
        printf("    Msg#{%s\n        };\n", fields)
    }
    # Reset
    parse_man_s=""
    fields=""
    variables=""
    bin_ctr=0
}

# Variables
$3 ~ /(Mandatory|Optional|Conditional)/ && inside_messages {
    iei_ref=trim($4)
    iei_name=iei_names[iei_ref]
    field_name=atom($2)
    var_name=atom_to_var(field_name)
    iei_code=iei_codes[iei_ref]
    iei_type=tag(iei_types[iei_ref])
    iei_instance=tolower(trim($5))
    if(iei_code == 254) {
        iei_length=2+iei_lengths[iei_ref]
    } else {
        iei_length=1+iei_lengths[iei_ref]
    }
    variable=sprintf("\n              {%s, {?GTPv2_IEI_%s, %s}, %s},", field_name, toupper(iei_name), iei_instance, atom(trim($3)))
    variables=variables variable
}

