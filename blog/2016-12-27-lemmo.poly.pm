#lang pollen

@(define-meta title "Lemmo")
@(define-meta tags (competition programming lemmings))

Wandering around in Wikipedia yesterday, I found this article: @emph{@link["https://en.wikipedia.org/wiki/Lemmings_(video_game)"]{Lemmings}}. Though I never play this game before,  I have played its open-source clone, @emph{@link["https://pingus.seul.org"]{Pingus}}, which is really enjoyable@numbered-note{I in fact have devised a pretty good (if not optimal) @link["https://www.youtube.com/watch?v=Wdl9bBUmU-k"]{strategy} for a level several years ago.}. This post is written to pay tribute to this wonderful game.

@/see-more[]

@/center{@img["https://upload.wikimedia.org/wikipedia/en/0/0c/Lemmings-BoxScan.jpg"]}

Anyway, the post is not really going to be about the game. @emph{Lemmings} also reminds me of something else too. Several years ago, I attempted to solve a programming puzzle and failed. Then I tried again a year after that, and for this time, I succeeded. It became one of my favorite puzzles because of its trickiness under a seemingly simple problem. This programming puzzle is known as @emph{Lemmo}@numbered-note{Here is the @link["http://theory.cpe.ku.ac.th/~pramook/ioi/2011/oct12_lemmo.pdf"]{original problem statement} (in Thai)}. Indeed, it's a yet another variant of @emph{Lemmings}.

Lemmo is a simple creature. All it does is merely walking. When hitting a wall, it turns around. When stepping over a space, it falls@numbered-note{Lemmo will survive however high it falls. This is not true in the original @emph{Lemmings}.}. It is guaranteed that the map that Lemmo is going to be in will have at least a space for every non-first floor. That is, Lemmo will definitely reach the first floor. The game ends when Lemmo steps over either a treasure box or a bottomless sewer drain, located on only the first floor. As you can anticipate, finding a treasure box is a win, while falling into a bottomless sewer drain is a lose. It is also guaranteed that there will be at least one treasure box or one sewer drain, and that there is no space on the first floor. That is, the game will always end.

Let @${r, c} be the number of rows and columns of the map respectively. Initially, Lemmo will start on the highest floor, which could be in @${c} different positions. It can either face to the left or to the right. Therefore, there are @${2c} initial configurations.

@(button "start" "Start") @(button "stop" "Stop") @(button "reset" "Reset")
@(canvas "game")

@/script|{
  const map = [
    "#####.###.##",
    "###.#####.##",
    "#.####.#####",
    "@#@$@$######"
  ];
  const canvas = document.getElementById("game");
  const ctx = canvas.getContext("2d");
  const rows = map.length;
  const cols = map[0].length;
  const height_gap = 40;
  const width_gap = 45;
  const center_width = width_gap / 2;
  const center_height = 2 * height_gap / 5;
  const margin_left = 10;
  const margin_top = 10;
  const color_white = "#FFFFFF";
  const color_yellow = "#CCCC00";
  const color_red = "#FF0000";
  const color_black = "#000000";
  const color_blue = "#0095DD";
  let pos_x = 0;
  let pos_y = 0;
  let dir = 1;
  const radius_ball = 10;

  function get_x(x) {
    return x * width_gap + margin_left;
  }

  function get_y(x) {
    return x * height_gap + margin_top;
  }

  let intv = null;
  let setup_step = true;

  function draw() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    for (var i = 0; i < rows; ++i) {
      for (var j = 0; j < cols; ++j) {
        if (map[i][j] != ".") {
          ctx.beginPath();
          ctx.rect(get_x(j), get_y(i) + ((height_gap * 4) / 5), width_gap - 1, height_gap / 5);
          ctx.fillStyle = color_red;
          ctx.fill();
          ctx.closePath();
          let color = color_white;
          if (map[i][j] == "#") {
            color = color_white;
          } else if (map[i][j] == "$") {
            color = color_yellow;
          } else if (map[i][j] == "@") {
            color = color_black;
          } else {
            throw new Error("Incorrect map value");
          }
          ctx.beginPath();
          ctx.arc(get_x(j) + center_width, get_y(i) + center_height, 9, 0, Math.PI*2);
          ctx.fillStyle = color;
          ctx.fill();
          ctx.closePath();
        }
      }
    }
    ctx.beginPath();
    if (dir == 1) {
      ctx.moveTo(get_x(pos_x) + center_width - (height_gap / 5), get_y(pos_y) + center_height - (height_gap / 5));
      ctx.lineTo(get_x(pos_x) + center_width - (height_gap / 5), get_y(pos_y) + center_height + (height_gap / 5));
      ctx.lineTo(get_x(pos_x) + center_width + (height_gap / 5), get_y(pos_y) + center_height);
    } else if (dir == -1) {
      ctx.moveTo(get_x(pos_x) + center_width + (height_gap / 5), get_y(pos_y) + center_height - (height_gap / 5));
      ctx.lineTo(get_x(pos_x) + center_width + (height_gap / 5), get_y(pos_y) + center_height + (height_gap / 5));
      ctx.lineTo(get_x(pos_x) + center_width - (height_gap / 5), get_y(pos_y) + center_height);
    } else {
      throw new Error("dir is not 1 or -1");
    }
    ctx.fillStyle = color_blue;
    ctx.fill();
    ctx.closePath();
  }

  draw();

  function act() {
    draw();
    if (map[pos_y][pos_x] == ".") {
      pos_y += 1;
    } else if (map[pos_y][pos_x] == "#") {
      pos_x += dir;
      if (pos_x < 0) {
        pos_x = 0;
        dir *= -1;
      }
      if (pos_x > cols - 1) {
        pos_x = cols - 1;
        dir *= -1;
      }
    } else {
      clearInterval(intv);
    }
  }

  function start_fun() {
    setup_step = false;
    if (intv == null) {
      intv = setInterval(act, 300);
    }
  }

  function stop_fun() {
    if (intv != null) {
      clearInterval(intv);
      intv = null;
    }
  }

  function reset_fun() {
    stop_fun();
    setup_step = true;
    pos_x = 0;
    pos_y = 0;
    dir = 1;
    draw();
  }

  document.getElementById("start").onclick = start_fun;
  document.getElementById("stop").onclick = stop_fun;
  document.getElementById("reset").onclick = reset_fun;

  document.addEventListener("keydown", function(e) {
    if (!setup_step) return;
    if(e.keyCode == 37) { // left
      if (dir == 1) dir = -1;
      else pos_x = Math.max(pos_x - 1, 0);
    }
    if(e.keyCode == 39) { // right
      if (dir == -1) dir = 1;
      else pos_x = Math.min(pos_x + 1, cols - 1);
    }
    draw();
  }, false);
}|

@notice{Use the following keys to setup the initial configuration: @kbds{Left} to move/face left, @kbds{Right} to move/face right. Then click @/code{Start} to begin the game.}

@task{Task 1: Write a program to find how many different initial configurations will lead Lemmo to a treasure box?}

@task{Task 2: Write a program to find the maximum number of different initial configurations that will lead Lemmo to a treasure box, provided that you are allowed to remove no more than one block from a non-first floor (that is, turning no more than one block into a space)?}

Here's the constraints:

@itemlist{
  @item{Time limit: 1 second}
  @item{Memory limit: 32MB}
  @item{@${1 \le r, c \le 1000}}
}

The input will be in the following format:

@highlight['text]|{
  #####.###.##
  ###.#####.##
  #.####.#####
  @#@$@$######
}|

where @/code|{@}| indicates a bottomless sewer drain, @/code{#} indicates a block on a floor, @/code{.} indicates a space, and @/code{$} indicates a treasure box.

In the above example, the configurations facing left in the column 6, 7, 8, 9 will lead to the leftmost bottomless sewer drain, while the rest will lead to the rightmost treasure box. However, if we remove the block at row 3, column 4, all configurations will lead to a treasure box. Therefore, the answer should be 20 and 24 respectively.

Following is my solution and explanation. Do not read if you wish not to see a spoiler. I have warned you.

.
.
.

@highlight['racket]|{
  #lang racket

  (define-syntax-rule (def whatever ...) (define whatever ...))
  (define-syntax-rule (def/memo (name arg ...) body ...)
    (def name (memoize (λ (arg ...) body ...))))
  (define-syntax-rule (ie0 condition true-expr ...)
    (cond [condition true-expr ...] [else 0]))

  (def (memoize f)
    (let ([dp (make-hash)]) (λ args (hash-ref! dp args (thunk (apply f args))))))

  (define-values (SPACE BLOCK WIN LOSE) (apply values (string->list ".#$@")))
  (def (normalize x) (if (equal? x SPACE) 0 1))
  (def (lookup t i j) (vector-ref (vector-ref t i) j))

  (def (calc ls)
    (def tab (list->vector
                (cons (build-vector (string-length (first ls)) (thunk* SPACE))
                      (map (λ (l) (list->vector (string->list l))) ls))))
    (def tab-sum (vector-map (λ (row-vector)
                                 (define-values (_ lst)
                                   (for/fold ([sum 0] [lst '(0)]) ([e row-vector])
                                     (def next-sum (+ sum (normalize e)))
                                     (values next-sum (cons next-sum lst))))
                                 (list->vector (reverse lst)))
                               tab))
    (def rows (vector-length tab))
    (def cols (vector-length (vector-ref tab 0)))
    (define-values (next prev) (values + -))
    (def (has-all? r i j)
      (= (+ j (- i) 1) (- (lookup tab-sum r (add1 j)) (lookup tab-sum r i))))
    (def (get-hmove c dir change)
      (def maybe-c (change c dir))
      (if (<= 0 maybe-c (sub1 cols)) (values maybe-c dir) (values c (- dir))))

    (def/memo (calc1 r c dir)
      (cond
        [(zero? r) 1]
        [else (define-values (p-c p-dir) (get-hmove c dir prev))
              (+ (ie0 (equal? SPACE (lookup tab (sub1 r) c)) (calc1 (sub1 r) c dir))
                 (ie0 (equal? BLOCK (lookup tab r p-c))      (calc1 r p-c p-dir)))]))
    (def/memo (win? r c dir)
      (match (lookup tab r c)
        [(== WIN) #t]
        [(== LOSE) #f]
        [(== SPACE) (win? (add1 r) c dir)]
        [(== BLOCK) (define-values (p-c p-dir) (get-hmove c dir next))
                    (win? r p-c p-dir)]))
    (def (delta r c dir)
      (ie0 (equal? BLOCK (lookup tab r c))
           (def proc (cond
                       [(and (win? (add1 r) c dir) (not (win? r c dir))) +]
                       [(and (not (win? (add1 r) c dir)) (win? r c dir)) -]
                       [else #f]))
           (ie0 proc
                (def over-calc
                  (ie0 (or (and (= dir -1) (has-all? r c (sub1 cols)))
                           (and (= dir 1)  (has-all? r 0 c)))
                       (calc1 r c (- dir))))
                (proc (- (calc1 r c dir) over-calc)))))

    (define-values (first-sol)
      (for*/fold ([sum 0]) ([c cols])
        (values (+ sum (ie0 (equal? WIN (lookup tab (sub1 rows) c))
                            (+ (calc1 (sub1 rows) c 1) (calc1 (sub1 rows) c -1)))))))
    (define-values (second-sol)
      (for*/fold ([sol 0]) ([r (sub1 rows)] [c cols]) ; only non-first floor
        (values (max sol (+ (delta r c -1) (delta r c 1))))))
    (values first-sol (+ first-sol second-sol)))
}|

Just about 70 lines, in Racket!

Then, to test:

@highlight['racket]|{
(require rackunit)

(def (check input sol1 sol2)
  (define-values (out1 out2) (calc input))
  (check-equal? out1 sol1)
  (check-equal? out2 sol2))

(check '("#####.###.##"
         "###.#####.##"
         "#.####.#####"
         "@#@$@$######") 20 24)
}|

does not raise any error, indicating that the answers are correct.

@/h2{Explanation}

@/h3{First Task}

The naive way to solve the first task is to simulate all possible configurations (in the order of @${O(c)}). Simulation is feasible because any initial configurations will lead to either a treasure box or a bottomless sewer drain eventually, so the simulation will terminate. However, a map like this:

@highlight['text]{
  #########.
  .#########
  #########$
}

would make Lemmo walk over every block (in the order of @${O(rc)}). Thus, it would run in @${O(rc^2)}. This would be too slow to finish in one second.

This is when dynamic programming comes to rescue. What we want to know is the number of ways that Lemmo will reach any treasure box from all possible configurations. Instead of computing that directly, we will generalize it by computing the number of ways that Lemmo will reach a certain position (facing in a certain direction) from all possible configurations. Then, we notice that the result of this computation relates to the number of ways to reach the previous step. For example, in this map:

@highlight['text]{
+------------
|
|  #.##.####.
|      ←
|  .#########
|
|  #########$
}

To be at @/code{←} (facing to the left, as suggested by the arrow), Lemmo must come from either the above (the above is a space, so it's possible that Lemmo will get here by falling from there), or from the right.

To illustrate, the number of initial configurations that could make Lemmo to be at @/code{←} is 15.

@highlight['text]{
    →→→→→→→→→
       ←←←←←←
+------------
|
|  #.##.####.
|      ←
|  .#########
|
|  #########$
}

which is merely the number of ways that Lemmo can reach the above (5)

@highlight['text]{
       ←←←←←
+------------
|      ←
|  #.##.####.
|
|  .#########
|
|  #########$
}

plus the number of ways that Lemmo can reach the right (10)

@highlight['text]{
    →→→→→→→→→
            ←
+------------
|
|  #.##.####.
|       ←
|  .#########
|
|  #########$
}

Let's now formalize it. Let @${W_{i, j, d}} be the number of ways to reach position at row @${i \in \{1,2,...,r\}}, column @${j \in \{1,2,...,c\}}, and @${d \in \{ L, R \}} indicates the direction that Lemmo is facing. This corresponds to the function @/code{calc1} in line 36.

@itemlist{
  @item{
    Base case: to simplify things, let's add another layer, row 0, to the map where I will refer to as "sky." Sky, as the name suggested, is all spaces (no block). We will then drop Lemmo from the sky instead. This will not affect the answer, since eventually Lemmo will fall to the original place anyway. However, we now have a clear base case, as there is exactly one way to reach it (which is by dropping it there). Therefore, @${W_{0, j, L} = W_{0, j, R} = 1} for all @${j}.

    Adding the sky layer (row 0) corresponds to line 18. The base case corresponds to line 38.
  }
  @item{
    Inductive case: if it's not the base case, then Lemmo can be at position @${i, j} facing in the direction @${d} by two ways:
    @numberlist{
      @item{From the space above: therefore, this is only applicable when the above is a space. Note that if Lemmo comes from the above, during the fall, it couldn't change a direction. Thus, if it is applicable, we know already what is the number of ways to get to the above: @${W_{i - 1, j, d}}. This corresponds to line 40.}
      @item{
        From behind (in the same row): this part is a little bit tricky. Suppose the behind position is a space, then Lemmo would fall to the lower floor. It can't jump to this position. Similarly, if the behind position is a treasure box or a bottomless sewer drain, then the game would have ended already. These are the cases that are not applicable.

        But moreover, there is one special case: suppose we are adjacent to a wall (either left or right wall) and facing out of the wall, then the behind position can't be within the wall. It would instead come from the exactly same position, but facing in the other direction. In the normal case however, the direction would still need to be maintained, as Lemmo will not switch the direction unless it's the special case.

        Thus, if it's applicable, the number of ways to get to the behind position is @${W_{i, j', d'}} where @${j'} is the behind position, and @${d'} is the direction in the behind position (@${d' \ne d} if it's a special case, but would be the same otherwise). This corresponds to line 41. Note that the function @/code{get-hmove} in line 32 is for computing the behind position (and its direction).
      }
    }

    Therefore:

    @${W_{i, j, d} = \begin{cases}
      W_{i - 1, j, d} + W_{i, j', d'} & \text{if both the above case}\\ & \text{and the behind case work}\\
      W_{i - 1, j, d} & \text{if only the above case works}\\
      W_{i, j', d'} & \text{if only the behind case works}\\
      0 & \text{otherwise}\\
    \end{cases}}

    which corresponds to line 40.
  }
}

Regarding the time complexity, for each @${i, j, d}, we compute @${W_{i, j, d}} in constant time, provided that other values in @${W} is calculated already. With @link["http://papl.cs.brown.edu/2016/Algorithms_That_Exploit_State.html#(part._.Abstracting_.Memoization)"]{memoization}, it takes @${O(rc)} to compute any (and all) @${W_{i, j, d}}.

To finish the first task, we need to calculate an answer, and this is straightforward: it's just the sum of number of ways to get to all treasure boxes, from both @${L} and @${R} directions. This corresponds to line 62.

@/h3{Second Task}

The naive way would be to try removing each block at a time in the map, and run the first task's algorithm to evaluate how many initial configurations that will lead to a treasure box increases after the removal. However, the number of blocks is in order of @${O(rc)}, and the algorithm in the first task takes @${O(rc)}. This would result in an @${O((rc)^2)}-algorithm. Bad!

One important fact is that Lemmo's walk is deterministic. Therefore, a position and the direction completely determines Lemmo's eventual destination. That is, it is possible compute @${A_{i, j, d} \in \{T_1, T_2, ..., T_t, F_1, F_2, ..., F_f\}} where @${T_1, T_2, ..., T_t} are all treasure boxes, and @${F_1, F_2, ..., F_f} are bottomless sewer drains, and @${i \in \{1,2,...,r\}, j \in \{1,2,...,c\}, d \in \{ L, R \}}. This, again, uses dynamic programming which is perhaps even easier than that in the first task, so I will skip the explanation.

Note, though, that this is even too detailed. We don't really want to know where exactly Lemmo will end up. We only want to know whether it will find a treasure box or it will fall into a bottomless sewer drain, as that's what we need to know to calculate the answer. Thus, we will refine the previous definition: we will compute @${A_{i, j, d} \in \{T, F\}} where @${T} indicates that the eventual destination is a treasure box, and @${F} indicates that the eventual destination is a bottomless sewer drains. This corresponds to function @/code{win?} at line 42. Note that it could be the case that the state @${(i, j, d)} might not be reachable at all, but suppose we somehow place Lemmo there, @${A_{i,j,d}} would determine the future status of Lemmo correctly.@numbered-note{@${A_{i,j,d}}  can also be used for computing the first task, but we will need both @${W_{i,j,d}} and @${A_{i,j,d}} so it doesn't matter which one we will use.}

What we are going to do will be similar to the naive way. We will try removing each block at a time in the map and evaluate how many initial configurations that will lead to a treasure box changes after the removal. However, we will not call the algorithm in the first task on the modified map. Instead, we will use the exactly same data from the first task that we have already to compute the answer.

Consider removing a block, say @${(h, k)} where @${h \in \{1,2,...,r\}, k \in \{1,2,...,c\}}. What is the impact? It definitely doesn't impact Lemmo that will not step on block @${(h, k)} in its travelling, since the map is otherwise the same. So, we only need to care about Lemmo which would step on @${(h, k)}. Say, in the old map Lemmo at @${(h, k, L)} will end up at @${T_5}, but after the removal of the block, it will end up at @${T_3}. But as mentioned before, this is too detailed. We don't care where Lemmo will end up exactly. Since in the old map Lemmo at @${(h, k, L)} will end up with a @${T} and in the new map it will end up with a @${T} too, this has no impact.

So a removal of a block will have an impact when @${A_{h,k,d} \ne A_{h+1,k,d}}, where @${A_{h,k,d}} indicates the eventual status of Lemmo stepping on the removed block in the old map (where the block is not removed yet), and @${A_{h+1,k,d}} indicates the evantual status of Lemmo stepping on a block below the removed block in the old map, which is the same as stepping on the removed block in the new map and then fall to the below. The impact is positive when @${A_{h,k,d} = F} and @${A_{h+1,k,d} = T} because it means previously in the old map, Lemmo would not find a treasure, but in the new map it will. The impact is negative when @${A_{h,k,d} = T} and @${A_{h+1,k,d} = F}.

But this still doesn't give us an answer. We want to know the delta of number of ways to reach a treasure box. Let @${N_{h,k,d}} indicates the number of ways that Lemmo will reach @${(h,k,d)} in the @emph{new} map (after removing block @${(h,k)}). The delta on a direction @${d} then would be @${N_{h,k,d}} when the impact is positive, @${-N_{h,k,d}} when the impact is negative, or @${0} otherwise. This partially corresponds to line 51 (continue reading the next paragraphs for a complete explanation). The total delta is the sum of these two directional deltas. And we just need to find @${(h, k)} that will maximize this delta. This corresponds to line 66.

How do we compute @${N_{h,k,d}}? Since the change occurs on row @${h}, it won't impact rows above @${h}. If we were to compute the table @${N} using dynamic programming, it would agree with @${W} until row @${h - 1}. So far so good. Now we try to compute @${N_{h,k,d}}. If all possible ways for Lemmo to reach @${(h,k)} is to fall from the above floor and then walk @emph{directly} to @${(h,k)}, as shown in the below map, then @${W_{h,k,d} = N_{h,k,d}} since this is exactly what @${W_{h,k,d}} would count as well.

@highlight['text]{
+---------------------------
|  .   .   .   .   .   .   .
|
|  ###########.#.###########
|             ←
|  ...###...##!###...###....
|
|  .   .   .   .   .   .   .
}

So @${N_{h,k,d} = W_{h,k,d}}, right?

Note that there is a condition above, which is, Lemmo must walk @emph{directly} to @${(h,k)}. This is almost always the case, but it is unfortunately false in some rare situations. Consider:

@highlight['text]{
+---------------------------
|  .   .   .   .   .   .   .
|
|  #############.########.##
|                      ←
|  ...###...###########!####
|
|  .   .   .   .   .   .   .
}

Here, @${N_{h,k,L} \ne W_{h,k,L}} because @${W_{h,k,L}} includes the situation when Lemmo steps on @${(h,k)} facing into the right wall, then bounce off the right wall, and step over @${(h,k)} again, this time facing out of the right wall. However, @${N_{h,k,L}} can't include this kind of situation because if the block is removed, Lemmo would fall to the below floor already when it steps on @${(h,k)} at the first time.

Now we know exactly when will this weird rare case happens: when Lemmo faces out of a wall, and would have just bounced off from the wall. That is, there are contiguous blocks starting from the @${(h,k)} to the wall. There are now two questions:

@itemlist{
  @item{
    How do we detect this special case efficiently?

    the naive way would be looping from @${(h,k)} to the wall and see if they are all blocks. However, number of blocks in a floor is in the order of @${O(c)}. We need to try removing all blocks where the number of all blocks are in the order of @${O(rc)} already, so this would result in @${O(rc^2)} which is too slow.

    And, you know how this will go: dynamic programming! They are all blocks if the current position is a block and the rest are all blocks. I in fact make it even simpler by observing the following: Let's say we maps @/code{#} to 1 and @/code{.} to 0.

    @highlight['text]{
      ##..###..#.####
    }

    would then be the following array @${B}:

    @highlight['text]{
      1 1 0 0 1 1 1 0 0 1 0 1 1 1 1
    }

    To see if it's all blocks from the third position from the right to the right wall, we need to see if they are all 1s. That is, if @${B_{13} + B_{14} + B_{15} = 1 + 1 + 1 \stackrel{?}{=} 3}, and in this case, it does, so it's all blocks.

    Let's try another one. We want to see if the fourth position from the left to the left wall are all blocks or not. That is, if @${B_1 + B_2 + B_3 + B_4 = 1 + 1 + 0 + 0 \stackrel{?}{=} 4}, and the answer is no, so it's not all blocks.

    But we still need to loop over to compute the sum of numbers! How is that gonna make a difference?

    Let's construct a prefix-sum table @${E} which in this case would be

    @highlight['text]{
      1 2 2 2 3 4 5 5 5 6 6 7 8 9 10
    }

    That is, @${E_k = \sum_{i = 1}^k B_i}. And by rewriting it to be in dynamic programming style, we obtain: @${E_k = \sum_{i = 1}^{k-1} B_i + B_k = E_{k - 1} + B_k}, where @${E_0 = 0}.

    Then, @${B_i + ... + B_j = \left(B_1 + ... + B_{i - 1} + (B_i + ... + B_j)\right) - (B_1 + ... + B_{i - 1})}. That is, @${B_i + ... + B_j = E_j - E_{i - 1}}.

    So detecting this special case can be done in @${O(1)} (provided that the prefix-sum is already constructed)!

    Building the prefix-sum table corresponds to line 20. Checking if it's all blocks corresponds to function @/code{has-all?} in line 30.
  }
  @item{
    How do we cope with it?

    Well, if we know that it's a special case, then we know that @${W_{h,k,d}} overcounts, because it includes @${W_{h,k,d'}} where @${d} is the direction facing out of the wall, and @${d'} is the direction facing in the wall. Therefore, @${N_{h,k,d}} is just @${W_{h,k,d} - W_{h,k,d'}}. This corresponds to line 60.
  }
}

The second task only needs to compute the table @${A} and the prefix-sum table. Just like the first task, both of these can be done in @${O(rc)} (one is a direct table-filling dynamic programming, and the other is memoization).

With these, we are able to compute the answer to the second task in @${O(rc)}.

FIN @emj{:)}
