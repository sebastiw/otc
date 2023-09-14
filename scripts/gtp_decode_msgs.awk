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

/^\* Chapter 7/ {
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

$3 !~ /For future use/ && active && in_msgs {
    msg_type=atom(trim($3))
    msg_code=trim($2)
    print "-define(GTP_MSG_TYPE_"toupper(msg_type)", "msg_code")."
    next;
}

# Type of message
/^\*\* 7[^ ]+/ && !/Information Elements/{
    inside_messages=1
    msg_type=atom(substr($0, length($1)+length($2)+3))
    msg_type=gensub("_messages$", "", 1, msg_type)
}
# !inside_messages { next; }

/^\*\* 7[^ ]+/ && /Information Elements/{
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
$3 !~ /^[[:space:]]*$/ && $4 !~ /.*(Spare|Reserved).*/ && active && inside_iei {
    iei_ref=trim($5)
    iei_name=atom(trim($4))
    iei_code=trim($2)
    iei_type=trim($3)
    iei_length_type=trim($6)
    iei_length=trim($7)

    iei_names[iei_ref]=iei_name
    iei_codes[iei_ref]=iei_code
    iei_types[iei_ref]=iei_type
    iei_lengths[iei_ref]=len(iei_length)
    iei_length_types[iei_ref]=iei_length_type
    printf("-define(GTPv1C_IEI_%s, %s).\n", toupper(iei_name), iei_code)
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
    printf("decode_msg(%s, IEIs) ->\n", header)
    fields=sprintf("               message_group => %s,\n", msg_type)
    next
}

# Empty lines in bottom of table finishes execution
/^$/ && active {
    FS=" "
    active=0
    printf("%s", parse_man_s)
    # Remove trailing comma and prefix whitespace
    optionals=trim(optionals)
    optionals=substr(optionals, 1, length(optionals)-1)
    fields=trim(fields)
    fields=substr(fields, 1, length(fields)-1)
    printf("    Fields = [%s],\n", optionals)
    printf("    Msg = decode_msg_fields(Fields, IEIs),\n")
    if(fields == "") {
        printf("    Msg;\n")
    } else {
        printf("    Msg#{%s};\n", fields)
    }
    # Reset
    optionals=""
    parse_man_s=""
    fields=""
    bin_ctr=0
}

# Optional variables
$3 ~ /(Mandatory|Optional|Conditional)/ && inside_messages {
    iei_ref=trim($4)
    iei_name=iei_names[iei_ref]
    iei_code=iei_codes[iei_ref]
    iei_type=tag(iei_types[iei_ref])
    iei_length=iei_lengths[iei_ref]
    iei_instance=tolower(trim($5))
    optional=sprintf("\n              {%s, %s, %s},", atom($2), iei_name, atom($3))
    optionals=optionals optional
}
