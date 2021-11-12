## Help functions ###################
function trim(s){
    sub(/^[ \t\r\n]+/, "", s)
    sub(/[ \t\r\n]+$/, "", s)
    return s
}

function atom(s){
    s=trim(tolower(s))
    gsub("[()']", "", s)
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

# Type of messages
/^\*\* 8.2 EPS/ {
    msg_type="emm"
}
/^\*\* 8.3 EPS/ {
    msg_type="esm"
}
/^\*\* 8.2 5GS/ {
    msg_type="5gmm"
}
/^\*\* 8.3 5GS/ {
    msg_type="5gsm"
}

# Every chapter (message)
/\*\*\*/ {
    header=tolower(substr($0, length($1)+length($2)+3))
    gsub(" ", "_", header)
    gsub("[-()]", "", header)
}

# Find the separator line in table
/\|--/ {
    active=1
    FS="|"
    next
}

# If not found yet, stop execution
active == 0 {
    next
}

# Empty lines in bottom of table finishes execution
/^$/ {
    FS=" "
    active=0
    # Remove trailing comma and prefix whitespace
    optionals=trim(optionals)
    optionals=substr(optionals, 1, length(optionals)-1)
    fields=trim(fields)
    fields=substr(fields, 1, length(fields)-1)
    map_fun_header=trim(map_fun_header)
    map_fun_header=substr(map_fun_header, 1, length(map_fun_header)-1)
    printf("encode_%s_msg(%s, #{%s} = Msg) ->\n", msg_type, header, map_fun_header)
    printf("%s", parse_man_s)
    printf("    Opts = [%s],\n", optionals)
    printf("    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),\n")
    if(fields == "") {
        printf("    OptBin;\n")
    } else {
        printf("    <<%s, OptBin/binary>>;\n", fields)
    }
    # Reset
    map_fun_header=""
    optionals=""
    parse_man_s=""
    fields=""
    bin_ctr=0
}

{
    field=atom($3)
}

# Optional variables
$5 ~ /O/ {
    optional=sprintf("\n            {%s, 16#%s, %s, %s},", field, hex($2), tag($6), len($7))
    optionals=optionals optional
    #fields=fields sprintf("               %s => maps:get(%s, Optionals, undefined),\n", field, field)
}

# Mandatory variables
$5 ~ /M/ {
    if(field == "protocol_discriminator" || field == "extended_protocol_discriminator") {
    } else if(field == "security_header_type") {
    } else if(field == "eps_bearer_identity" || field == "pdu_session_id") {
    } else if(field == "procedure_transaction_identity" || field == "pti") {
    } else if(field ~ /.*message_(type|identity)/) {
    } else {
        iei_type=tag($6)
        if(iei_type == "lv" || iei_type == "lve") {
            maybe_len=""
        } else {
            maybe_len=", " len($7)
        }
        if(field == "spare_half_octet") {
            # first one comes from security header type
            if(bin_ctr > 0) {
                bin_ctr++
                parse_man=sprintf("    Bin%s = <<0:4>>,  %% %s\n",
                                  bin_ctr, field)
                parse_man_s=parse_man_s parse_man
                fields=fields sprintf("Bin%s/bitstring, ", bin_ctr)
            }
        } else {
            bin_ctr++
            map_fun_header=map_fun_header sprintf("%s := %s, ", field, atom_to_var(field))
            parse_man=sprintf("    Bin%s = otc_l3_codec:encode_%s(%s%s, <<>>),\n",
                              bin_ctr, iei_type, atom_to_var(field), maybe_len)
            parse_man_s=parse_man_s parse_man
            fields=fields sprintf("Bin%s/bitstring, ", bin_ctr)
        }
    }
}
