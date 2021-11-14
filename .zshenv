# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# GPG
export GPG_TTY=$(tty)

# Use nvim
export EDITOR='nvim'
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

read -r -d '' TIMEFMT <<EOM
%J   %U  user %S system %P cpu %*E total
  avg shared (code):         %X KB
  avg unshared (data/stack): %D KB
  total (sum):               %K KB
  max memory:                %M KB
  page faults from disk:     %F
  other page faults:         %R
EOM

# Use ripgrep and FZF
if type rg &>/dev/null; then
  alias grep=rg
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Java
JHOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home"
if [ -f "$JHOME" ]; then
  export JAVA_HOME="$JHOME"
  export PATH="$JAVA_HOME/bin:$PATH"
  export CPPFLAGS="-I$JAVA_HOME/include"
fi

# Compilers
export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/llvm/include"
IARM="/Applications/ARM/arm-none-eabi/include"
if [ -f "$IARM" ]; then
  export CPPFLAGS="$CPPFLAGS -I$IARM"
fi
ILLVM="/usr/local/opt/llvm/include"
if [ -f "$ILLVM" ]; then
  export CPPFLAGS="$CPPFLAGS -I$ILLVM"
fi
FLLVM="/usr/local/opt/llvm/lib"
if [ -f "$FLLVM" ]; then
  export LDFLAGS="$FLLVM"
fi

# Path
export PATH="/Applications/ARM/bin:$PATH"
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PATH="/usr/local/opt/python@3.7/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export WASMTIME_HOME="$HOME/.wasmtime"

export PATH="$WASMTIME_HOME/bin:$PATH"

export VULKAN_SDK="/usr/local/lib/vulkan_sdk/1.2.189.0"
export VK_ICD_FILENAMES=$VULKAN_SDK/macOS/share/vulkan/icd.d/MoltenVK_icd.json
export VK_LAYER_PATH=$VULKAN_SDK/macOS/share/vulkan/explicit_layer.d

export PATH="$HOME/.scripts:$PATH"

export PATH="/opt/homebrew/opt/openal-soft/bin:$PATH"
