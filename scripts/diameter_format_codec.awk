## Help functions ###################
function trim(s){
    sub(/^[ \t\r\n]+/, "", s)
    sub(/[ \t\r\n]+$/, "", s)
    return s
}

## Execution ###################

/^[[:space:]]*;/ {
    print;
    next;
}

/;/ {
    comment=gensub(/.*(;.*)/, " \\1", 1)
}
!/;/ {
    comment=""
}

/^@(id|vendor|name)/ {
   printf("%-10s%-6s%s\n", $1,$2,$3);
   next;
}

/^@/ {
    section=tolower($1);
    print
    next;
}

/::[[:space:]]*=/ {
    sub(/::[[:space:]]*=/, "::=");
}

/^[a-zA-Z0-9-]+/ && "@avp_types" == section {
    avpcols=gensub(/^([a-zA-Z0-9-]+)[[:space:]]+([0-9]+)[[:space:]]+([a-zA-Z0-9]+)[[:space:]]+([MV-]*).*$/, "\\1|\\2|\\3|\\4", "g", trim($0));
    split(avpcols, avp, "|");
    printf("%-60s%-5s%-18s%-2s%s\n", avp[1], avp[2], avp[3], avp[4], comment);
    next;
}

/^[[:space:]]*$/ && ("@messages" == section || "@grouped" == section) {
    printf("%s", obligatory_avps)
    printf("%s", mandatory_avps)
    printf("%s", optional_avps)
    obligatory_avps=""
    mandatory_avps=""
    optional_avps=""
}

/::=/ && "@messages" == section {
    split($0, arr, "::=");
    name=trim(arr[1]);
    name=gensub(/<?[[:space:]]*([a-zA-Z0-9 -])[[:space:]]*>?/, "\\1", "g", name);
    header=trim(arr[2])
    header=gensub(/<[[:space:]]*[dD]iameter [hH]eader[[:space:]]*:(.*)[[:space:]]*>/, "\\1", "g", header)
    split(header, head, ",")
    cmd=trim(head[1])
    h=""
    for(a in head) {
        f=trim(head[a])
        if(f != "") {
            h=h f", "
        }
    }
    h=substr(h, 1, length(h)-2)
    print name,"::=","<Diameter Header:",h ">"
    next;
}

/::=/ && "@grouped" == section {
    split($0, arr, "::=");
    name=trim(arr[1]);
    name=gensub(/<?[[:space:]]*([a-zA-Z0-9 -])[[:space:]]*>?/, "\\1", "g", name);
    header=trim(arr[2])
    header=gensub(/^.*<[[:space:]]*[aA][vV][pP] [hH]eader[[:space:]]*:[[:space:]]*(.+)[[:space:]]*>[[:space:]]*$/, "\\1", "g", header)
    split(header, head, ",")
    cmd=trim(head[1])
    h=""
    for(a in head) {
        f=trim(head[a])
        if(f != "") {
            h=h f", "
        }
    }
    h=substr(h, 1, length(h)-2)
    print trim(name),"::=","<AVP Header:",h ">"
    next;
}

/[<{\[].*[>}\]]/ && ("@messages" == section || "@grouped" == section) {
    multiple=gensub(/^([0-9]*)[[:space:]]*(\*?)[[:space:]]*([0-9]*)[[:space:]]*.*$/, "\\1|\\2|\\3", "g", trim($0))
    field=gensub(/^.*([<{\[])[[:space:]]*(.*)[[:space:]]*([>}\]]).*$/, "\\1|\\2|\\3", "g", trim($0))
    split(multiple, pre, "|")
    split(field, fi, "|")
    type=trim(fi[1])
    l = sprintf("%10s%-1s%-3s%s %s %s", trim(pre[1]), trim(pre[2]), trim(pre[3]), type, trim(fi[2]), trim(fi[3]))
    l = sprintf("%-80s%s\n", l, comment)
    sub(/[[:space:]]*\n/, "\n", l)
    if(type == "<") {
        obligatory_avps=obligatory_avps l
    } else if (type == "{") {
        mandatory_avps=mandatory_avps l
    } else if (type == "[") {
        optional_avps=optional_avps l
    } else {
        print "Could not determine type of "$0 > "/dev/stderr"
    }
    next;
}

!/@enum/ && !/@define/ && ("@enum" == section || "@define" == section) {
    enum=gensub(/-/, "_", "g", $1)
    value=gensub(/[().,]/, "", "g", $2)
    if("" != enum) {
        printf("%-60s%2s%s\n", enum, value, comment)
    } else {
        print
    }
    next;
}


{ print }
