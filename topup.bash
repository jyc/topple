randstr()
{
    cat /dev/urandom | LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1
}

if [ -z "$1" ]; then
    VERSION_FILE=".topple-version"
else 
    VERSION_FILE="${1%%/}/.topple-version"
fi

if [ -e "$VERSION_FILE" ]; then
    echo "$(randstr)" > $VERSION_FILE
else
    echo "No $VERSION_FILE found. Create one?"
    read -p "[Y/n] " CREATE
    if [ "$CREATE" != "n" ]; then
        echo "$(randstr)" > $VERSION_FILE
        echo "Created."
    fi
fi
