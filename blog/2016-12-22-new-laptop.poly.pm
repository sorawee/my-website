#lang pollen

@(define-meta title "My New Laptop & Settings")
@(define-meta tags (ubuntu thinkpad post))

I switched from Ubuntu to Macbook Pro since 2013 and was very happy with it. About 5 months ago, the screen was broken, so I planned to buy a new one. Knowing that the new version will have a major change, I was very excited and decided to wait until it's released in order to buy it. To my disappointment, @link["http://www.theverge.com/2016/11/7/13548052/the-macbook-pro-lie"]{it incredibly sucks}@numbered-note{I would have loved it if I were not a programmer -- the missing @kbds{Esc} and traditional USB ports matter a lot to me}.

A lot of my friends suggested me to get a Thinkpad X1 Yoga which looks really cool. I waited until Thanksgiving to get a big discount. The laptop arrived in early December. The first thing I did is to install Ubuntu 16.10 because I hate Windows! @emj{:P}

Following are my settings. I write these particularly for myself in the future so that when I need to set things up again, I won't have to spend a lot of time figuring out what to do!

@see-more

First of all, install @code{git} and set up the directory that contains all git projects.

@highlight['sh]|{
sudo apt install git
mkdir ~/git
cd git
git clone git@github.com:sorawee/dotfiles.git
git clone git@github.com:brownplt/pyret-lang.git

# For spacemacs
git clone --depth 1 --branch release https://github.com/adobe-fonts/source-code-pro.git
}|

I use Spacemacs as my main editor. Here's how I set it up:

@highlight['sh]|{
sudo apt install emacs25 # or emacs26 or whatever you want
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

mkdir ~/emacs
mkdir ~/.fonts
wget http://www.neilvandyke.org/scribble-emacs/scribble.el -O ~/emacs/scribble.el
cd
ln -s git/dotfiles/.spacemacs .
cp ~/git/source-code-pro/OTF/* ~/.fonts
sudo fc-cache
cp /usr/share/applications/emacs25.desktop ~/.local/share/applications/
}|

Then edit @code{~/.local/share/applications/emacs25.desktop}. Change the @code{Exec} line to @code{Exec=/usr/bin/emacsclient -c -a "" -F "((fullscreen . maximized))" %F} and change the @code{Name} to whatever short name you like. Now you should be able to load emacs GUI properly.

Now that we have a decent editor, we can set the rest up.

@highlight['sh]|{
## run `sudo apt update` as appropriate

# get a good terminal
sudo add-apt-repository ppa:webupd8team/terminix
sudo apt install tilix
gsettings set org.gnome.desktop.default-applications.terminal exec 'tilix'

# get zsh
sudo apt install zsh
zsh # don't generate .zshrc
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^(README.md|zshrc|zprestorc)(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
for rcfile in "${ZDOTDIR:-$HOME}"/git/dotfiles/dotfiles/*; do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
chsh -s /bin/zsh
}|

The directory @code{bin} contains all local executable files that I would like to install and can be linked from the dotfiles directory as well.

@highlight['sh]|{
ln -s git/dotfiles/bin bin
}|

There are a bunch of other programs I want to install

@highlight['sh]|{
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

sudo add-apt-repository ppa:plt/racket
sudo apt install racket
raco pkg install pollen
sudo apt install python-setuptools
sudo easy_install --upgrade Pygments

sudo apt install dropbox
sudo apt install xournal

curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt install -y nodejs

sudo add-apt-repository ppa:peek-developers/stable
sudo apt install peek
}|

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

For Spotify, after the installation, we need to set the scale to make it display properly with HiDPI: copy @code{/usr/share/applications/spotify.desktop} to @code{~/.local/share/applications/}, then edit the @code{Exec} line to be @code{Exec=spotify --force-device-scale-factor=2 %U}.

Slack and Skype work very well without any additional configuration.

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

I also want to use @kbd{AltR Left} and @kbd{AltR Right} for @kbd{Home} and @kbd{End}. Similarly, I want to use @kbd{AltR Up} and @kbds{AltR Down} for @kbds{PageUp} and @kbds{PageDn}. This can be done by assigning @kbds{AltR} a mode switch and make arrow keys mode-switch-sensitive. That is:

@filebox-highlight["~/.xsession" 'bash]|{
xmodmap -e "keycode 108 = Mode_switch"
xmodmap -e "keycode 113 = Left NoSymbol Home"
xmodmap -e "keycode 114 = Right NoSymbol End"
xmodmap -e "keycode 111 = Up NoSymbol Prior"
xmodmap -e "keycode 116 = Down NoSymbol Next"
}|

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

Set @code{disable_touchpads} to @code{True}. For @code{rotate_pens}, it depends on the desktop environment that you are using. For example, I need to set this to @code{True} for Unity, but @code{False} for GNOME (Ubuntu 17.10). Then:

@highlight['bash]|{
  cp /opt/thinkpad-yoga-scripts/systemd/yoga-rotate@.service /lib/systemd/system/
  # somehow systemd stuff in /usr/lib/systemd/system/ or
  # /usr/lib/systemd/user/ doesn't work, so use this instead
  systemctl start yoga-rotate@$USER.service
}|

FIN @emj{:)}
