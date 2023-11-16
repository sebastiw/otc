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

/* Chapter 8/ {
    print "%% 3GPP TS TS 29.118 version 17.0.0";
    print "";
    print "%% 9.2 Message type";
    start_msgs=1
}

start_msgs_table && /^$/ {
    start_msgs_table=0
    start_msgs=0
    FS=" "
}

start_msgs_table {
    gsub(/-/, "_", $3);
    sub(/SGsAP/, "SGSAP_MSGT", $3);
    print "-define("toupper(trim($3))", "trim($2)")."
}

start_msgs && /--------/ {
    start_msgs_table=1
    FS="|"
}

start_msgs {
    next;
}


/* 9 Information element coding/ {
    print "";
    print "%% 9.3 Information element identifiers";
    start_ieis=1
}

start_iei_table && /^$/ {
    start_iei_table=0
    start_ieis=0
    FS=" "
}

start_iei_table {
    iei=hex($3);
    gsub(/ /, "_", iei);
    iei="SGSNAP_IEI_"iei
    ref=trim($4)
    ieis[ref]=iei
    print "-define("iei", "trim($2)")."
}

start_ieis && /--------/ {
    start_iei_table=1
    FS="|"
}

start_ieis {
    next;
}


/** 8/ {
    start_msg=1
    msg=tolower(trim($3))
    gsub(/-/, "_", msg);
    sub(/sgsap_/, "", msg);
}

start_msg_table && /^$/ {
    start_msg_table=0
    start_msg=0
    FS=" "
}

start_msg_table {
    refs[msg][start_msg_table]=trim($3)
    nams[msg][start_msg_table]=atom(tag($2))
    typs[msg][start_msg_table]=tolower(trim($5))
    lens[msg][start_msg_table]=len($6)

    switch (tolower(trim($4))) {
        case "m":
            mand[msg][start_msg_table]="mandatory"
            break
        case "o":
            mand[msg][start_msg_table]="optional"
            break
        case "c":
            mand[msg][start_msg_table]="conditional"
            break
    }
    start_msg_table++;
}

start_msg && /--------/ {
    start_msg_table=1
    FS="|"
}


END {
    print "";
    print "%% 6.3 Transport layer";
    print "-define(SCTP_PPI_SGSNAP, 0).";
    print "-define(SGSNAP_PORT, 29118).";
    print "";

    for(msg in refs) {
        print "allowed_ieis("msg") ->";
        bin_ctr=0;
        printf("    [");
        i=""
        for(iei in refs[msg]) {
            if("message_type" != nams[msg][iei]) {
                ref=refs[msg][iei]
                i=i"     {"nams[msg][iei]", ?"ieis[ref]", "mand[msg][iei]", "lens[msg][iei]"},\n";
            }
        }
        print substr(i, 6, length(i)-7)"];"
    }

    print "";
    print "decode_msg(MT, IEIs) ->";
    print "    Allowed = allowed_ieis(MT),";
    print "    parse_ieis(Allowed, IEIs).";
    print "";
    print "encode_msg(MT, Msg) ->";
    print "    Allowed = allowed_ieis(MT),";
    print "    compose_ieis(Allowed, Msg).";
}
