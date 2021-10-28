# YADM Dotfiles
My dotfiles using [yadm](https://yadm.io), macOS only for now because I only have it set up with brew.
To install, run the following in terminal:
```bash
curl -fLo /tmp/yadm https://github.com/TheLocehiliosan/yadm/raw/master/yadm && chmod a+x /tmp/yadm

echo -n Password: 
read -s password
echo

/tmp/yadm clone "https://willemml:$password@github.com/willemml/dotfiles"
password=""
```
#!/bin/bash

