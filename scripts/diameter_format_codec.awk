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

/^@(id|vendor|name)/ {
   printf("%-10s%-6s%s\n", $1,$2,$3);
   next;
}

/^@/ {
    section=tolower($1);
    print
    next;
}

/::=/ && "@messages" == section {
    name=trim($1);
    name=gensub(/<?[[:space:]]*\([a-zA-Z0-9-]\)[[:space:]]*>?/, "\\1", "g", name);
    header=trim($0)
    header=gensub(/^.*<[[:space:]]*[dD]iameter [hH]eader:(.*)[[:space:]]*>$/, "\\1", "g", header)
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
    name=trim($1);
    name=gensub(/<?[[:space:]]*\([a-zA-Z0-9 -]\)[[:space:]]*>?/, "\\1", "g", name);
    header=trim($0)
    header=gensub(/^.*<[[:space:]]*[aA][vV][pP] [hH]eader:[[:space:]]*(.+)[[:space:]]*>[[:space:]]*$/, "\\1", "g", header)
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
    field=gensub(/^.*([<{\[])[[:space:]]*(.*)[[:space:]]*([>}\]].*)$/, "\\1|\\2|\\3", "g", trim($0))
    split(multiple, pre, "|")
    split(field, fi, "|")
    printf("%10s%-1s%-3s%s %s %s\n", trim(pre[1]), trim(pre[2]), trim(pre[3]), trim(fi[1]), trim(fi[2]), trim(fi[3]))
    next;
}

!/@enum/ && "@enum" == section {
    enum=gensub(/-/, "_", "g", $1)
    if("" != enum) {
        printf("%-60s%2s\n", enum, $2)
    } else {
        print
    }
    next;
}


{ print }
