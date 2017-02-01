#lang pollen

@(define-meta title "My New Website")
@(define-meta tags (website blog post))

Ahem. This is my 7@^{th} attempt to write a blog... Hopefully it's not gonna fail like last time!

In the previous attempts, I hosted my blog with blog sites such as Wordpress or Blogspot. For this time, as you can see, I host it in my own website (powered by Github Pages), so it will (hopefully) be different!

And, you see, the blog content is not that important. The most crucial thing is to get the technology right@numbered-note{I'm being sarcastic @emj{:P}}. What I'm using right now is called @link["http://pollenpub.com/"]{Pollen} markup, which is pretty much a domain-specific language based on Racket's @link["https://docs.racket-lang.org/scribble/reader.html"]|{@-syntax}|. Pollen sets up a server which recompiles files automatically whenever it detects file changes. The markup itself is very cool in a sense that you just write whatever you want to write with arbitrary made-up semantics tags, which is very pleasant to write using the said syntax. Then, you can go back and define and format what those tags are supposed to be. The programming part is equally pleasant because it has @link["https://docs.racket-lang.org/xml/"]{X-Expression} which is a cool version of XML. Pollen supports outputting multiple targets: @LaTeX[], @code{html}, plain text, or whatever you want to output. Basically, you program the document, so you have an unlimited power to do whatever you want.

@/see-more[]

Now it's time to show off what it can do:

@numberlist[
  @item{@${\LaTeX}: @${x = \frac{b \pm \sqrt{b^2 - 4ax}}{2a}}}
  @item{@code{code} highlight:
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
  }
  @item{Have I demonstrated that it can have emoji? @emj{:)}}
  @item{Margin note: it works well even on mobile phones@numbered-note{Really, try it!}.}
  @item{@link["/feed.xml"]{RSS} is implemented in Racket and fully customizable@numbered-note{Disclaimer: I am not the one I wrote it. I just copied and adapted it to fit my need.}.}
  @item{Magic keyboard keys and menu: like go to @menu{Menu > Games > Fire Emblem} and press @kbds{Up Up Down Down Left Right Left Right A B Select Start}}
  @item{@link["/"]{The home page} can extract the summary paragraphs from posts. I write this by myself!}
  @item{The tag system is written in Racket (by me @emj{:)}), though it is not integrated to Pollen@numbered-note{@link["https://mstill.io"]{Malcolm} also implemented another tag system, but I do no know how it works -- haven't taken a look at it.}}
  @item{Disqus: See below!}
]
