#lang pollen

@(define-meta title "My New Laptop & Settings")
@(define-meta tags (ubuntu thinkpad post))

I switched from Ubuntu to Macbook Pro since 2013 and was very happy with it. About 5 months ago, the screen was broken, so I planned to buy a new one. Knowing that the new version will have a major change, I was very excited and decided to wait until it's released in order to buy it. To my disappointment, @link["http://www.theverge.com/2016/11/7/13548052/the-macbook-pro-lie"]{it incredibly sucks}@numbered-note{I would have loved it if I were not a programmer -- the missing @kbds{Esc} and traditional USB ports matter a lot to me}.

A lot of my friends suggested me to get a Thinkpad X1 Yoga which looks really cool. I waited until Thanksgiving to get a big discount. The laptop arrived in early December. The first thing I did is to install Ubuntu 16.10 because I hate Windows! @emj{:P}

Following are my settings. I write these particularly for myself in the future so that when I need to set things up again, I won't have to spend a lot of time figuring out what to do!

@/see-more[]

@highlight['sh]|{
# get a good terminal
sudo add-apt-repository ppa:webupd8team/terminix
sudo apt update
sudo apt install tilix
gsettings set org.gnome.desktop.default-applications.terminal exec 'tilix'

# get zsh for history substring search and other cool stuff
sudo apt install zsh
zsh
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
rm -f .zshrc
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
chsh -s /bin/zsh
}|

The configuration of @code{.zpreztorc} is straightforward. The only major thing I did is to add the history substring search plugin. Note that @code{'prompt'} must be the last one.

@filebox-highlight["~/.zpreztorc" 'bash]|{
...
'completion' \
'history-substring-search' \
'prompt'
}|

I really like the default prompt of @code{zprezto} which is @code{sorin}, but I want normal path display and want to have current time shown as the right prompt. I also don't like crazy git status. So I changed:

@filebox-highlight["~/.zprezto/modules/prompt/functions/prompt_sorin_setup" 'diff]|{
@@ -39,7 +39,8 @@ function prompt_sorin_pwd {
     _prompt_sorin_pwd="$MATCH"
     unset MATCH
   else
-    _prompt_sorin_pwd="${${${${(@j:/:M)${(@s:/:)pwd}##.#?}:h}%/}//\%/%%}/${${pwd:t}//\%/%%}"
+    #_prompt_sorin_pwd="${${${${(@j:/:M)${(@s:/:)pwd}##.#?}:h}%/}//\%/%%}/${${pwd:t}//\%/%%}"
+    _prompt_sorin_pwd="${pwd}"
   fi
 }

@@ -80,7 +81,8 @@ function prompt_sorin_precmd {
   prompt_sorin_pwd

   # Define prompts.
-  RPROMPT='${editor_info[overwrite]}%(?:: %F{1}⏎%f)${VIM:+" %B%F{6}V%f%b"}'
+  RPROMPT='[%D{%L:%M:%S %p}]'
+  #RPROMPT='${editor_info[overwrite]}%(?:: %F{1}⏎%f)${VIM:+" %B%F{6}V%f%b"}'

   # Kill the old process of slow commands if it is still running.
   if (( _prompt_sorin_precmd_async_pid > 0 )); then
}|

@code{agnoster}, @code{pure}, and @code{powerline} also look interesting, but I like single line more than double line, so right now I will stick with @code{sorin}.

Here's my additional settings for @code{.zshrc}:

@filebox-highlight["~/.zshrc" 'bash]|{
# use terminal emacs as default
alias emacs='emacs -nw'
# use it like this: run_long_command; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" \
"$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
# reset the network
alias rnet='sudo systemctl restart NetworkManager.service'
}|

I like OS X's @code{open} a lot. Here we have @code{xdg-open}, but it vomits a lot of text while running in background. Thus, I created:

@filebox-highlight["~/bin/open" 'bash]|{
#!/usr/bin/zsh

xdg-open $@ &> /dev/null
}|

There are a bunch of other programs I want to install

@highlight['sh]|{
sudo apt install emacs24 # or emacs25, 26, whatever you want

sudo apt install atom

sudo apt install clipit

sudo apt install liferea

sudo apt install powertop tlp tp-smapi-dkms acpi-call-dkms
sudo tlp start

sudo apt install unity-tweak-tool
sudo add-apt-repository ppa:numix/ppa
sudo apt install numix-icon-theme-square numix-gtk-theme
# and then set the theme and icon, and get 2x2 workspace

sudo apt install gnome-settings-daemon gnome-control-center

sudo apt install chromium-browser
# vivaldi is also great

sudo apt install gimp # current version is gimp2.8
wget https://github.com/jedireza/gimp-hidpi/archive/master.zip
unzip master.zip -d ~/.gimp-2.8/themes
# need to add the theme in gimp's setting too
rm master.zip

sudo apt install texlive-full texstudio

sudo apt install caffeine

sudo add-apt-repository ppa:jconti/recent-notifications
sudo apt install indicator-notifications

sudo apt install rustc

sudo add-apt-repository "deb https://cli-assets.heroku.com/branches/stable/apt ./"
curl -L https://cli-assets.heroku.com/apt/release.key | sudo apt-key add -
sudo apt install heroku
gem install bundler
sudo apt install ruby-dev
sudo apt install libmysqlclient-dev

sudo add-apt-repository ppa:nilarimogard/webupd8
sudo apt install gloobus-preview gloobus-sushi

sudo apt install libavcodec-extra

# installing ttf-mscorefonts-installer will fail, do it manually afterward
sudo apt install ubuntu-restricted-extras

# manually install ttf-mscorefonts-installer
# 1) download all files from
#    https://sourceforge.net/projects/corefonts/files/the%20fonts/final/
#    except wd97vwr32.exe to ~/Downloads/fonts
# 2) setup
sudo dpkg-reconfigure ttf-mscorefonts-installer
# 3) need to clean up failed download to avoid annoying popup
cd /usr/share/package-data-downloads/
sudo rm ttf-mscorefonts-installer

sudo apt install adobe-flashplugin

# after setting up gpg
sudo apt install pass
pass init GPG_KEY
pass git init

sudo apt install psensor

sudo apt install redshift redshift-gtk

sudo add-apt-repository ppa:webupd8team/java
sudo apt install oracle-java8-installer

sudo add-apt-repository ppa:plt/racket
sudo apt install racket
raco pkg install pollen
sudo apt install python-setuptools
sudo easy_install --upgrade Pygments

sudo apt install dropbox
sudo apt install xournal

curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt install -y nodejs
}|

@link["github.com/phw/peek"]{Peek} is a screen recorder program which produces GIF as the output. However, it needs an additional configuration to avoid freezing computer:

@highlight['bash]|{
  # the current version is 0.8.0.
  wget https://github.com/phw/peek/releases/download/v0.8.0/peek-0.8.0-Linux.deb
  sudo dpkg -i peek-0.8.0-Linux.deb
  rm peek-0.8.0-Linux.deb
}|

Then edit @code{/usr/share/applications/com.uploadedlobster.peek.desktop}'s Exec to @code{TMPDIR=/var/tmp MAGICK_TMPDIR=/var/tmp MAGICK_TEMPORARY_PATH=/var/tmp peek}.

As a non-English speaker, I use dictionaries a lot. I also want it to be offline. The best program I could fine (which is also the same one I used 4 years ago) is @code{goldendict}. Here's the setup:

@highlight['bash]|{
mkdir ~/dict
cd ~/dict
# I suppose it will no longer be developed, so let's just hard code the version number
# EN->TH dictionary
wget http://ftp.psu.ac.th/pub/stardict/stardict-lexitron-et-2.4.2.tar.bz2
# TH->EN dictionary
wget http://ftp.psu.ac.th/pub/stardict/stardict-lexitron-te-2.4.2.tar.bz2
tar -xf stardict-lexitron-et-2.4.2.tar.bz2
tar -xf stardict-lexitron-te-2.4.2.tar.bz2
# thanks to http://jsomers.net/blog/dictionary and
# eduardosanchez.me/2015/09/07/installing-websters-revised-unabridged-dictionary-on-ubuntu-gnulinux/
wget https://s3.amazonaws.com/jsomers/dictionary.zip
unzip dictionary.zip
cd dictionary
tar -xf stardict-dictd-web1913-2.4.2.tar.bz2
mv stardict-dictd-web1913-2.4.2 ~/dict
cd ~/dict
rm -rf dictionary *.tar.bz2
# also need to add ~/dict as a directory to search for dictionaries
# *recursively* in goldendict
}|

For Spotify, after the installation, we need to set the scale to make it display properly with HiDPI: change @code{/usr/share/applications/spotify.desktop} to use @code{Exec=spotify --force-device-scale-factor=2 %U}.

For Skype, I find that the version in the repository uses Qt 4, which sucks (especially on HiDPI). It's better to get the beta version which is Qt-5-based from the @link["https://web.skype.com/en/"]{webapp}

Slack works very well without any additional configuration.

Now, I happen to often run stuff that will hang the computer, which would require me to hard-reset it if I don't have any other plan. Although I'm using SSD now which means there is little risk of damaging the disk when hard-resetting, I still don't want to do it. Thus, I will enable two keyboard shortcuts for the soft reset:

@itemlist[
  @item{
    @code{sudo dpkg-reconfigure keyboard-configuration} in order to set @kbds{Ctrl Alt Backspace} to restart X server.
  }
  @item{
    Edit @code{/etc/sysctl.conf} and set @code{kernel.sysrq=1} so that we can use @link["https://en.wikipedia.org/wiki/Magic_SysRq_key"]{Magic SysRq key}
  }
]

Last but not least, Thinkpad tweaking!

Thinkpad is overall very nice, but the keyboard layout sucks. Who the hell thinks it's a good idea to remove "play/pause" key and replace it with those dumb "search" button? This is however easy to get back. Just go to @menu{Keyboard > Shortcuts > Sound and Media} and set "Play (or play/pause)", "Previous track", and "Next track" to new keys.

The next one is a little bit more difficult. In Macbook Pro, I would use @kbds{Fn Left} and @kbds{Fn Right} for @kbds{Home} and @kbds{End}, and @kbds{Fn Up} and @kbds{Fn Down} for @kbds{PageUp} and @kbds{PageDn}. In Thinkpad, all @kbds{Home}, @kbds{End}, @kbds{PageUp}, and @kbds{PageDn} are keys! @kbds{PageUp} and @kbds{PageDn} seem to be in the right place, but @kbds{Home} and @kbds{End} are at the damned top row! Why?!

Ideally, I would want @kbds{Fn Left} and @kbds{Fn Right} back, but after a lot of Googling, I found that @kbds{Fn} in Thinkpad is a special key which is not really configurable. Fine. But when I program, I tend to use @kbds{Home} and @kbds{End} @emph{a lot}, so I thought, I want to swap @kbds{Home}, @kbds{End} with @kbds{PageUp}, @kbds{PageDn}. This is really easy to do using @code{xkb}:

@filebox-highlight["/usr/share/X11/xkb/symbols/pc" 'diff]|{
@@ -74,11 +74,11 @@ xkb_symbols "editing" {
 	symbols[Group1]= [ Pause, Break ]
     };
     key  <INS> {	[  Insert		]	};
-    key <HOME> {	[  Home			]	};
-    key <PGUP> {	[  Prior		]	};
+    key <HOME> {	[  Prior		]	};
+    key <PGUP> {	[  Home			]	};
     key <DELE> {	[  Delete		]	};
-    key  <END> {	[  End			]	};
-    key <PGDN> {	[  Next			]	};
+    key  <END> {	[  Next			]	};
+    key <PGDN> {	[  End			]	};

     key   <UP> {	[  Up			]	};
     key <LEFT> {	[  Left			]	};
}|

But when I want to read articles or news, I would use @kbds{PageUp} and @kbds{PageDn} a lot. Pressing keys on the top row would not be ideal. Thus, I created:

@filebox-highlight["~/bin/switch-reading-mode" 'bash]|{
#!/usr/bin/zsh

if [[ -f ~/.switch-reading-mode ]]; then
  xmodmap -e "keycode 110 = Prior NoSymbol Prior"
  xmodmap -e "keycode 115 = Next NoSymbol Next"
  xmodmap -e "keycode 112 = Home NoSymbol Home"
  xmodmap -e "keycode 117 = End NoSymbol End"
  rm ~/.switch-reading-mode
  sleep 0.2
  killall notify-osd
  notify-send "Normal mode"
else
  xmodmap -e "keycode 110 = Home NoSymbol Home"
  xmodmap -e "keycode 115 = End NoSymbol End"
  xmodmap -e "keycode 112 = Prior NoSymbol Prior"
  xmodmap -e "keycode 117 = Next NoSymbol Next"
  touch ~/.switch-reading-mode
  sleep 0.2
  killall notify-osd
  notify-send "Reading mode"
fi
}|

Then, in @menu{Keyboard > Shortcuts}, create a custom command which invokes @code{~/bin/switch-reading-mode} when pressing @kbds{CapsLock}. Indeed, when entering the reading mode, the Capslock would be on, but in the reading mode, you don't type, so it's fine! @emj{:)} Note that the reason I choose Capslock is that it has the physical light indicator on the keyboard, meaning that I will know whether I am in the reading mode or not without having to try pressing @kbds{Home} or other keys.

Life of Thinkpad in Ubuntu also kinda sucks. Fingerprint reader @link["https://forums.lenovo.com/t5/forums/v3_1/forumtopicpage/board-id/Special_Interest_Linux/thread-id/7920/page/1"]{doesn't work}. @strike{Neither is Tablet mode auto-rotation. In fact, I couldn't even connect the accelerometer although I'm sure there is one because it works in Windows. So I need to manually tell Ubuntu to rotate screen, which is kinda awkward. I make it easier by calling this script (which also does other things like disabling trackpad, etc.):}

@outdated{
@filebox-highlight["/usr/local/bin/rotate-screen" 'bash]|{
#!/usr/bin/zsh
# adapted from Ruben Barkow: https://gist.github.com/rubo77/daa262e0229f6e398766

#### configuration
# find your Touchscreen and Touchpad device with `xinput`
TouchscreenDevice='Wacom Co.,Ltd. Pen and multitouch sensor Finger touch'
TouchpadDevice='SynPS/2 Synaptics TouchPad'

touchpadEnabled=$(xinput --list-props "$TouchpadDevice" | awk '/Device Enabled/{print $NF}')
screenMatrix=$(xinput --list-props "$TouchscreenDevice" | awk '/Coordinate Transformation Matrix/{print $5$6$7$8$9$10$11$12$NF}')

# Matrix for rotation
# ⎡ 1 0 0 ⎤
# ⎜ 0 1 0 ⎥
# ⎣ 0 0 1 ⎦
normal='1 0 0 0 1 0 0 0 1'
normal_float='1.000000,0.000000,0.000000,0.000000,1.000000,0.000000,0.000000,0.000000,1.000000'

#⎡ -1  0 1 ⎤
#⎜  0 -1 1 ⎥
#⎣  0  0 1 ⎦
inverted='-1 0 1 0 -1 1 0 0 1'
inverted_float='-1.000000,0.000000,1.000000,0.000000,-1.000000,1.000000,0.000000,0.000000,1.000000'

# 90° to the left
# ⎡ 0 -1 1 ⎤
# ⎜ 1  0 0 ⎥
# ⎣ 0  0 1 ⎦
left='0 -1 1 1 0 0 0 0 1'
left_float='0.000000,-1.000000,1.000000,1.000000,0.000000,0.000000,0.000000,0.000000,1.000000'

# 90° to the right
#⎡  0 1 0 ⎤
#⎜ -1 0 1 ⎥
#⎣  0 0 1 ⎦
right='0 1 0 -1 0 1 0 0 1'

if [ "$1" == "-u" ]
then
  echo "Upside down"
  xrandr -o inverted
  xinput set-prop "$TouchscreenDevice" 'Coordinate Transformation Matrix' $inverted
  xinput disable "$TouchpadDevice"
elif [ "$1" == "-t" ]
then
  echo "90° to the left"
  xrandr -o left
  xinput set-prop "$TouchscreenDevice" 'Coordinate Transformation Matrix' $left
  xinput disable "$TouchpadDevice"
else
  echo "Back to normal"
  xrandr -o normal
  xinput set-prop "$TouchscreenDevice" 'Coordinate Transformation Matrix' $normal
  xinput enable "$TouchpadDevice"
fi
}|

Then create three icons (don't forget to @code{chmod +x} the script first):

@filebox-highlight["/usr/share/applications/rotate-screen-tent.desktop" 'text]|{
[Desktop Entry]
Type=Application
Terminal=false
Icon=un-reboot
Name=Tent Mode
Exec=/usr/local/bin/rotate-screen -u
Categories=Utility;
}|

@filebox-highlight["/usr/share/applications/rotate-screen-tablet.desktop" 'text]|{
[Desktop Entry]
Type=Application
Terminal=false
Icon=wireframing
Name=Tablet Mode
Exec=/usr/local/bin/rotate-screen -t
Categories=Utility;
}|

@filebox-highlight["/usr/share/applications/rotate-screen-laptop.desktop" 'text]|{
[Desktop Entry]
Type=Application
Terminal=false
Icon=system-config-displayca
Name=Laptop Mode
Exec=/usr/local/bin/rotate-screen
Categories=Utility;
}|

And drag all of these to the Unity bar. Also drag @code{/usr/share/applications/onboard.desktop} to the Unity bar for on-screen keyboard for non-laptop mode.}

I managed to get auto-rotation to work! First:

@highlight['bash]{
  sudo apt install iio-sensor-proxy
  systemctl start iio-sensor-proxy.service
}

Then, run @code{monitor-sensor}. It should look like:

@highlight['bash]{
  $ monitor-sensor
      Waiting for iio-sensor-proxy to appear
  +++ iio-sensor-proxy appeared
  === Has accelerometer (orientation: normal)
  === Has ambient light sensor (value: 0.908000, unit: lux)
  Light changed: 1.557000 (lux)
  Light changed: 13.365001 (lux)
  ...
}

If it doesn't appear like this, there are two possibilties:

@numberlist[
  @item{The kernel you are using is @link["https://github.com/hadess/iio-sensor-proxy/issues/71"]{buggy}!. Download and install @link["http://kernel.ubuntu.com/~kernel-ppa/mainline/v4.9.3/"]{kernel version 4.9.3} or later to fix this issue.}
  @item{There's a weird @link["https://github.com/hadess/iio-sensor-proxy/issues/124"]{bug} that @code{monitor-sensor} won't work unless you sleep your laptop and wake it up... So... do that.}
]

When @code{monitor-sensor} is working, install @code{@link["https://github.com/admiralakber/thinkpad-yoga-scripts"]{thinkpad-yoga-scripts}} by the following instructions. Note that @code{thinkpad-yoga-scripts} has a lot of features that we do not need. For example, "Disabling of Touchscreen with proximity of Wacom digitizer" is working already for us. So we will not follow the instructions in the README. Instead:

@highlight['bash]|{
  sudo -s
  git clone https://github.com/admiralakber/thinkpad-yoga-scripts /opt/thinkpad-yoga-scripts
  nano /opt/thinkpad-yoga-scripts/rotate/thinkpad-rotate.py
}|

Set both @code{rotate_pens} and @code{disable_touchpads} to @code{True}. Then:

@highlight['bash]|{
  cp /opt/thinkpad-yoga-scripts/systemd/yoga-rotate@.service /lib/systemd/system/
  # somehow systemd stuff in /usr/lib/systemd/system/ or
  # /usr/lib/systemd/user/ doesn't work, so use this instead
  systemctl start yoga-rotate@$USER.service
}|

FIN @emj{:)}
