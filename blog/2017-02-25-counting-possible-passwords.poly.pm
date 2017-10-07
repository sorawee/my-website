#lang pollen

@(define-meta title "Counting Possible Passwords")
@(define-meta tags (dynamic-programming programming))

Jack showed this @link["http://portal.cs.oag.state.tx.us/OAGStaticContent/portal/login/help/listPasswordRules.htm"]{the rules for password naming} in a website to me. Here's an excerpt:

@bquote{
  @numberlist[
    @item{The password must be exactly 8 characters long.}
    @item{It must contain at least one letter, one number, and one of the following special characters.}
    @item{The only special characters allowed are: @"@" # $}
    @item{A special chaacter must not be located in the first or last position.}
    @item{Two of the same characters sitting next to each other are considered to be a "set." No "sets" are allowed. Example: rr, tt}
    @item{Avoid using names, such as your name, user ID, or the name of your company or employer.}
    @item{Other words that cannot be used are Texas, child, and the months of the year.}
    @item{A new password cannot be too similar to the previous password.

      @itemlist[
        @item{Example: previous password - abc#1234; unacceptable new password - acb$1243}
        @item{Characters in the first, second, and third positions cannot be identical. (abc*****)}
        @item{Characters in the second, third, and fourth positions cannot be identical. (*bc#****)}
        @item{Characters in the sixth, seventh, and eighth positions cannot be identical. (*****234)}
      ]}
    @item{A password can be changed voluntarily (no Help Desk assistance needed) once in a 15-day period. If needed, the Help Desk can reset the password at any time.}
    @item{The previous 8 passwords cannot be reused.}
  ]

  One way to create a password is creative spelling and substitution. Examples:

  @numberlist[
    @item{phuny#2s}
    @item{fish#1ng}
    @item{t0pph@"@"ts}
    @item{run$4you}
    @item{ba#3ries}
  ]
}

I'm terrified.

@see-more

This reminds me of an xkcd comic @link["https://xkcd.com/936/"]{"Password Strength"}. As the comic states, through 20 years of effort, we've successfully trained everyone to use passwords that are hard for humans to remember, but easy for computers to guess. For this website, it's even worse because the website itself forces users to abide these non-sense rules.

What we are more interested, however, is to count how many possible passwords there are! We will consider only rule 1 to 5, as the rest is about specific names or previous passwords which would be hard to define mathematically.

It might be possible to use combinatorics to solve the problem. However, as I study computer science, I will use computer to solve the problem!

First, write a naive program to count the answer:

@highlight['python]|{
  import string as st

  def count_naive(num_symbols, num_digits, num_letters, length):
      symbol_chars = "@#$!%^&*()_+=-`~{}[]:?/>.<,;"[:num_symbols]
      digit_chars = st.digits[:num_digits]
      letter_chars = st.ascii_letters[:num_letters]
      all_chars = symbol_chars + digit_chars + letter_chars
      string = [-1]

      def generate(n):
          if n == 0:
              # print(''.join(string[1:]))
              return (any(c in string for c in symbol_chars) and
                      any(c in string for c in digit_chars) and
                      any(c in string for c in letter_chars))
          total = 0
          for c in all_chars:
              if (n == 1 or n == length) and c in symbol_chars: continue
              if string[-1] == c: continue
              string.append(c)
              total += generate(n - 1)
              string.pop()
          return total
      return generate(length)

    print(count_naive(2, 3, 4, 5)) #=> 11472
}|

Pretty much, this program goes over the entire possible strings@numbered-note{All @code{continue} statements would do some pruning, but that won't help much} and chooses only valid ones. Now, we can compute the answer to the original question. It's just @code{count_naive(3, 10, 52, 8)}, as the number of symbols is 3 (#, $, and @"@"); the number of digits, 10 (0-9); number of letters, 52 (a-z, A-Z), right?

The answer is yes, but as the program goes over the entire possible strings, it would take a really long time to compute the answer. In particular, the algorithm's runtime is @${O(s + d + l)^n} where @${s} is number of symbols, @${d} is number of digits, @${l} is number of letters, and @${n} is the length. With this, we can approximate the real runtime. The number of all possible strings is @${(3 + 10 + 52)^8 = 318644812890625}. Assuming that our computer can execute 1 billion instructions per second, it would take @${(3 + 10 + 52)^8 (\frac{1}{1000000000})(\frac{1}{60})(\frac{1}{60})(\frac{1}{24}) \approx 3.68} days to count them all. I don't want to wait that long!

To reduce the running time, notice that all numbers/letters/symbols look the same. Instead of iterating over @${(3 + 10 + 52)^8} states, we can compact these states, iterating over only @${(1 + 1 + 1)^8 = 6561} states!

@highlight['python]|{
  def count(num_symbols, num_digits, num_letters, length):
      SYMBOL = 0
      DIGIT = 1
      LETTER = 2
      groups = [0, 1, 2]
      sizes = [num_symbols, num_digits, num_letters]
      string = [-1]
      total = 0
      cnt = 1

      def generate(n):
          nonlocal cnt, total
          if n == 0:
              if {0, 1, 2} <= set(string):
                  total += cnt
              return
          for g in groups:
              if (n == 1 or n == length) and g == SYMBOL: continue
              mult = sizes[g] - (1 if string[-1] == g else 0)
              if mult == 0: continue
              string.append(g)
              cnt *= mult
              generate(n - 1)
              cnt //= mult
              string.pop()
      generate(length)
      return total
}|

The above algorithm has runtime @${O(3^n)}! The tricky part is excluding string which has the same characters adjacent to each other. To do this, we notice that if the group (type of the character -- letter, digit, or symbol) matches the previous group, then instead of having all choices, we would lose one because it's the same as the previous character.@numbered-note{This, by the way, is the first time I use Python 3's @code{nonlocal}.}

Computing the answer is now feasible (only one second!).

@highlight['bash]|{
  $ python3 -i count.py
  >>> count(3, 10, 52, 8)
  46082343914880
}|

Assuming that this answer is correct, we can see that all these weird rules reduce @${(3 + 10 + 52)^8 - 46082343914880 = 272562468975745} possible passwords!

How can we be sure that this new program is indeed correct? We can create an oracle to test it on smaller inputs, using the slower but definitely correct algorithm as the tester.

@highlight['python]|{
  def oracle(f1, f2):
      lim = 5
      n = 100
      for trial in range(n):
          print('Trial #{}'.format(trial))
          num_symbols = random.randint(1, lim)
          num_digits = random.randint(1, lim)
          num_letters = random.randint(1, lim)
          length = random.randint(1, lim)
          args = [num_symbols, num_digits, num_letters, length]
          answer1 = f1(*args)
          answer2 = f2(*args)
          if answer1 != answer2:
              print(("symbols: {}, digits: {}, letters: {}, len: {}\n" +
                     "f1: {}, f2: {}").format(*(args + [answer1, answer2])))
              return False
      return True

  print(oracle(count, count_naive)) #=> True
}|

Now we have an @${O(3^n)} algorithm. Can we do better? The answer is yes! We will use dynamic programming to help improving the runtime to linear!!

Let @${T[n, g, s, d, l]} indicates the number of possible words with length @${n}, ending with a character in group @${g \in \set{0, 1, 2}}, containing at least one symbol iff @${s = 1}, containing at least one digit iff @${d = 1}, and containing at least one letter iff @${l = 1}, where @${s, d, l \in \set{0, 1}}. The recurrence equation now becomes apparent.@numbered-note{I'm just too lazy to explain, but it's not hard to see. If I were to write a textbook, this would be a good opportunity to write the infamous "left as an exercise"}

@highlight['python]|{
  def count_dp(num_symbols, num_digits, num_letters, length):
      SYMBOL = 0
      DIGIT = 1
      LETTER = 2
      sizes = [num_symbols, num_digits, num_letters]
      groups = [0, 1, 2]
      binary = [0, 1]
      dp_template = {(g, hs, hd, hl): 0 for g in groups
                                        for hs in binary
                                        for hd in binary
                                        for hl in binary}
      dp_new = dict(dp_template)
      dp_new[SYMBOL, 1, 0, 0] = 0 # the first character can't be a symbol
      dp_new[DIGIT,  0, 1, 0] = sizes[DIGIT]
      dp_new[LETTER, 0, 0, 1] = sizes[LETTER]

      for i in range(1, length):
          dp_old = dp_new
          dp_new = dict(dp_template)
          for g in groups:
              space = [(hs, hd, hl) for hs in binary
                                    for hd in binary
                                    for hl in binary]
              for hs, hd, hl in [t for t in space if t[g] != 0]:
                  for prevg in groups:
                      dp_new[g,hs,hd,hl] += (
                          dp_old[prevg, hs, hd, hl] +
                          (dp_old[prevg, 0, hd, hl] if g == SYMBOL and hs else 0) +
                          (dp_old[prevg, hs, 0, hl] if g == DIGIT and hd else 0) +
                          (dp_old[prevg, hs, hd, 0] if g == LETTER and hl else 0)
                      ) * (sizes[g] - (1 if g == prevg else 0))
      return dp_new[DIGIT, 1, 1, 1] + dp_new[LETTER, 1, 1, 1]

  print(oracle(count_dp, count)) #=> True
}|

This yields a linear time algorithm. Additionally, the algorithm takes constant amount of memory!

FIN @emj{:)}
