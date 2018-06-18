#lang pollen

@(define-meta title "Spacemacs, i3, and Manjaro")
@(define-meta tags (spacemacs manjaro i3 thinkpad post))

After I upgraded my computer to Ubuntu 18.04, I was fed up with how laggy things are, especially for @link["https://atom.io/"]{Atom} and @link["https://wiki.gnome.org/Projects/GnomeShell"]{Gnome Shell}. Since all of my finals are over, I decided to try out new things.

@see-more

@subsection{Spacemacs}

I started by learning @link["https://www.vim.org/"]{Vim}, as it loads much faster than Atom. Although I am still not used to the modal editing, I find it very appealing and think it might make my life more productive, so I decided to give it a try@numbered-note{Although I find the key @kbds{j} and @kbd{k} very unintuitive. They should be swapped!}.

However, Vim is not really extensible, so I tried to find an alternative. @link["https://www.gnu.org/software/emacs/"]{Emacs}, which I have been using for a while, is an obvious choice, but it is not a modal editor... Except that Emacs is extensible enough to have the @link["https://www.emacswiki.org/emacs/Evil"]{evil mode} which makes Emacs become a modal editor. I in fact used @link["http://spacemacs.org/"]{Spacemacs} which has a bunch of packages preinstalled, including the evil mode.

There's a problem, however. Spacemacs makes Emacs @emph{very} slow. Fortunately, I found an @link["https://medium.com/@bobbypriambodo/blazingly-fast-spacemacs-with-persistent-server-92260f2118b7"]{article} that tells me that I have been using Emacs wrong for my whole life: I should set up an emacs server, so that emacs doesn't need to reload things again when I open a client.

@subsection{i3 and Manjaro}

As mentioned above, I would like to switch from Gnome Shell to something else. As all people in the programming language lab uses a tiling window manager (Jack and Justin use @link["http://xmonad.org/"]{XMonad} and Preston uses @link["http://swaywm.org/"]{Sway}), I feel compelled to do the same. I decided to use @link["https://i3wm.org/"]{i3} because I feel it doesn't need to be configured as much as XMonad, and Sway, which uses Wayland, seems to be relatively unstable compared to i3.

Jack has been trying to convince me to install @link["https://www.archlinux.org/"]{Arch Linux}. While I find rolling release great, I really don't want to set everything up by myself, so @link["https://manjaro.org/"]{Manjaro Linux}, which is advertised as "a user-friendly Linux distribution based on ... Arch", is thus a natural candidate to look at. I find its i3 edition really great because unlike fresh installation of i3, where nothing works (sound keys, brightness keys, etc.), Manjaro i3 edition has everything working out of the box.

@subsection{Setup}

First things first, let's use the fastest mirror site:

@highlight['sh]|{
sudo pacman-mirrors -g
sudo pacman -Syyu
}|

Then, install a sensible terminal. I don't like @link["http://software.schmorp.de/pkg/rxvt-unicode.html"]{urxvt} as it doesn't support things out of the box. I am currently using @link["https://github.com/thestinger/termite"]{termite} instead.

@highlight['sh]|{
pms termite
}|

Next, set up the directory that contains all git projects and install dotfiles.

@highlight['sh]|{
mkdir ~/git
cd git
git clone git@github.com:sorawee/dotfiles.git
pms racket # need Racket for the dotfiles installer
pms racket-docs # Racket documentation needs to be installed separately
cd dotfiles
racket setup.rkt
}|

As mentioned above, I use Spacemacs as my main editor. Here's how I set it up:

@highlight['sh]|{
pms emacs
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
mkdir ~/emacs
wget http://www.neilvandyke.org/scribble-emacs/scribble.el -O ~/emacs/scribble.el # Scribble mode
emacs # call emacs explicitly to install packages. Don't forget to rebuild epdfview when a dialog appears
}|

I also want to use @code{zsh} as my default shell.

@highlight['sh]|{
zsh
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^(README.md|zshrc|zprestorc)(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
chsh -s /bin/zsh
}|

We need to restart the computer to make it take effect. Next, install a bunch of programs:

@highlight['sh]|{
pms firefox # firefox, cause I don't like pale moon
pms yay     # yay
pms glipper # clipboard manager
pms polybar # alternative top bar
pms mpd     # needed by polybar to work correctly
pms xf86-input-wacom # so that wacom works

# fonts
pms adobe-source-code-pro-fonts # for Spacemacs and polybar
pms ttf-font-awesome # for polybar
yay -S nerd-fonts-complete # for polybar
yay -S ttf-th-sarabun-new # Thai font

pms rofi    # use rofi instead of dmenu
pms evince  # for a PDF viewer that has "print to file"
pms aspell-en # fix Spacemacs' error
pms npm # Pyret stuff
sudo npm install -g tern # fix Spacemacs' error
pms xournal # xournal
pms gimp    # gimp
pms liferea # feed reader
pms playerctl # so that we can play/pause and next/previous
pms jdk10-openjdk # for Alloy development
pms eclipse-java # for Alloy development
yay -S slack-desktop # Slack
pms gnome-common # dependency for iio-sensor-proxy
yay -S iio-sensor-proxy # for accelerometer
pms texlive-most # TeX
pms pass # Password Manager
pms xorg-xwininfo # for ~/bin/i3-get-window-criteria
yay -S peek # GIF recorder
yay -S skypeforlinux-stable-bin # Skype
yay -S spotify # Spotify

# my blog
raco pkg install pollen
raco pkg install libuuid

# fingerprint reader
yay -S libfprint-vfs0090-git
pms fprintd

sudo apt install adobe-flashplugin
sudo apt install redshift redshift-gtk
sudo apt install dropbox

}|

I want to install @code{goldendict} along with the dictionaries as usual:

@highlight['bash]|{
pms goldendict
mkdir ~/dict
cd ~/dict
wget http://ftp.psu.ac.th/pub/stardict/stardict-lexitron-et-2.4.2.tar.bz2
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
Slack and Skype work very well without any additional configuration.

Last but not least, Thinkpad tweaking!

@highlight['bash]|{
  yay -S thinkpad-yoga-scripts-git
  e -n /opt/thinkpad-yoga-scripts/rotate/thinkpad-rotate.py
}|

Set both @code{disable_touchpads} and @code{rotate_pens} to @code{True}. Then:

@highlight['bash]|{
  systemctl enable yoga-rotate@$USER.service --now
  systemctl enable yoga-backlight.service --now
}|

FIN @emj{:)}
