#lang pollen

@(define-meta title "Markov Chain Monte Carlo")
@(define-meta tags (probability programming))

I just finished my 6@^{th} semester! One of the classes that I took is @emph{Probabilistic Methods in Computer Science}. Some topics in the class are really useful. Unfortunately, as it's a pure theory class, there is no programming at all. Don't get me wrong. I in fact really like theory classes, but for this one, I really think that programming (either as an assignment or a demonstration in the class) would make the class more interesting, and would give students a moment of "wow, that's cool!" more. So in this post, I will write about the Markov Chain Monte Carlo method (by random walk), the topic I like most, along with a demonstration using Python.

@see-more[]

The Markov Chain Monte Carlo method is a general approach to sample a data from a desired distribution. For me, I use this all the time for software verification. The most basic way to test that a program is correct is to test it with all possible inputs, but in practice, this is nearly impossible as there are a huge amount of possible inputs. Instead, we can randomly generate a small amount of inputs and test the program with them. Hopefully, the generated inputs will be good representations of all possible inputs, and if the program is correct on those inputs, probably it's correct on other inputs too. There are also other reasons one might need to sample data. Perhaps for estimating something. Perhaps for simulating something. Or perhaps you want to play Catan but don't have dice.

We will focus on uniform sampling, since that's what I am interested in, and will talk about sampling from a desired distribution only briefly.

@/h3{Introduction}

Let's say that you want to sample an integer in [1, 6] uniformly. This is easy. In Python:

@highlight['python]|{
import random

def sample_number():
  """
  Random a number in range [1, 6]
  """
  return random.randint(1, 6)

print(sample_number())
}|

Let's say that you want to uniformly sample a directed acyclic graph (DAG) with a given number of edges and vertices (say, for testing that your topological sort algorithm is correct). Now, there's no Python function to return a sample. One way would be to enumerate all possible DAGs with @${n} vertices and @${m} edges and random an index. This is of course impractical. However, there is a way to do this nicely.

@highlight['python]|{
def sample_dag(n, m):
  """
  Random a DAG with n vertices, m edges.
  The output is a list of edges.
  """
  E = set()
  for i in range(m):
    x = 0
    y = 0
    while x >= y or (x, y) in E:
      x = random.randint(0, n - 1)
      y = random.randint(0, n - 1)
    E.add((x, y))
  shuffled = random.sample(range(n), k=n) # or use Fisher–Yates shuffle
  return [(shuffled[x], shuffled[y]) for x, y in E]

print(sample_dag(10, 20))
}|

The algorithm is divided into two parts. The first part (line 4 to line 10) constructs the "shape" which guarantees that the graph must be a DAG since for each edge, @${x < y}. Each shape can be constructed in @${m!} ways, so all shapes are equally likely. The second part (line 12 to line 14) relabeled all vertices uniformly randomly.

What about something more complicated, like CNF clauses, or independent sets? What if we change from "exactly @${n}" to "at most @${n}" for some parameter @${n}? Now we don't really see an obvious way to go... For CNF, we might try:

@highlight['python]|{
def sample_CNF(n, v):
  """
  Random a CNF with at most n clauses, n literals, v variables
  The output is a list of clauses where each clause is
  a list of literals. Positive literal is a positive number,
  while negative literal is a negative number.
  """
  num_clauses = random.randint(0, n)
  CNF = []
  for c in range(num_clauses):
    num_lits = random.randint(0, n)
    CNF.append([random.choice([-1, 1]) * random.randint(1, v)
                for i in range(num_lits)])
  return CNF

print(sample_CNF(10, 20))
}|

But this won't have a uniform distribution. For example, with probability @${\frac{1}{n+1}}, we will get an empty CNF, while it's very less likely to get something like @tt{[[-1], [2], [], [3, -1]]}

@/h3{First MCMC}

Now we will go back to the first example "sample an integer in [1, 6] uniformly". However, this time we will use the Markov Chain Monte Carlo method.

Consider this code:

@highlight['python]|{
def sample_number_v1():
  now = 1
  t = 100
  for i in range(t):
    dir = random.choice([-1, 1])
    if (dir == -1 and now == 1) or (dir == 1 and now == 6):
      continue
    now += dir
  return now

print(sample_number_v1())
}|

The scheme is that initially, we are at some state (here, state 1). We will then "walk" randomly for @${t} times (here, @${t = 100}), and then return the state that we are in. For each step:

@itemlist[
  @item{Uniformly choose a direction (increasing/decreasing) to go (with probability 1/2) @itemlist[
    @item{If the new state is valid, go}
    @item{Otherwise, stay still}
  ]}
]

If we write the probability transition matrix explicitly, we see the following: @/table[
  @/tr[@/th{src\dest}
               @/th{1}   @/th{2}   @/th{3}   @/th{4}   @/th{5}   @/th{6}]
  @/tr[@/th{1} @/td{0.5} @/td{0.5} @/td{0}   @/td{0}   @/td{0}   @/td{0}]
  @/tr[@/th{2} @/td{0.5} @/td{0}   @/td{0.5} @/td{0}   @/td{0}   @/td{0}]
  @/tr[@/th{3} @/td{0}   @/td{0.5} @/td{0}   @/td{0.5} @/td{0}   @/td{0}]
  @/tr[@/th{4} @/td{0}   @/td{0}   @/td{0.5} @/td{0}   @/td{0.5} @/td{0}]
  @/tr[@/th{5} @/td{0}   @/td{0}   @/td{0}   @/td{0.5} @/td{0}   @/td{0.5}]
  @/tr[@/th{6} @/td{0}   @/td{0}   @/td{0}   @/td{0}   @/td{0.5} @/td{0.5}]
]

To see the result, we use @code{Counter} from @code{collections} to see the frequencies of numbers. Here's the result:

@highlight['python]|{
  >>> from collections import Counter
  >>> Counter(sample_number_v1() for i in range(1000))
  Counter({5: 183, 2: 182, 3: 175, 6: 156, 1: 154, 4: 150})
  >>> Counter(sample_number_v1() for i in range(1000))
  Counter({6: 179, 5: 177, 2: 171, 4: 166, 1: 157, 3: 150})
  >>> Counter(sample_number_v1() for i in range(1000))
  Counter({4: 189, 2: 180, 5: 175, 3: 164, 1: 147, 6: 145})
}|

Compare this with the straightforward way:

@highlight['python]|{
  >>> Counter(sample_number() for i in range(1000))
  Counter({3: 187, 1: 173, 5: 169, 6: 160, 2: 159, 4: 152})
  >>> Counter(sample_number() for i in range(1000))
  Counter({6: 186, 4: 184, 1: 166, 3: 166, 2: 152, 5: 146})
  >>> Counter(sample_number() for i in range(1000))
  Counter({2: 184, 4: 170, 1: 169, 6: 164, 3: 159, 5: 154})
}|

@/h3{Requirements}

Can this scheme fail? The answer is unfortunately yes!

@/h4{Irreducibility}

Of course, you want the sampling function to be able to return all possible states. Since we start at state @tt{s} for some @tt{s}, we want @tt{s} to be able to reach every state.

Consider a variant of @code{sample1v1}:

@highlight['python]|{
def sample1v2():
  now = 1
  t = 100
  for i in range(t):
    dir = random.choice([-1, 0, 1])
    if (dir == -1 and now == 1) or (dir == 1 and now == 6):
      continue
    now += dir
  return now
}|

For each step:

@itemlist[
  @item{Uniformly choose a direction (increasing/decreasing) to go (with probability 1/3) @itemlist[
    @item{If the new state is valid, go}
    @item{Otherwise, stay still}
  ]}
  @item{Otherwise, stay still}
]

If we write the probability transition matrix explicitly, we see the following: @/table[
  @/tr[@/th{src\dest}
               @/th{1}   @/th{2}   @/th{3}   @/th{4}   @/th{5}   @/th{6}]
  @/tr[@/th{1} @/td{2/3} @/td{1/3} @/td{0}   @/td{0}   @/td{0}   @/td{0}]
  @/tr[@/th{2} @/td{1/3} @/td{1/3} @/td{1/3} @/td{0}   @/td{0}   @/td{0}]
  @/tr[@/th{3} @/td{0}   @/td{1/3} @/td{1/3} @/td{1/3} @/td{0}   @/td{0}]
  @/tr[@/th{4} @/td{0}   @/td{0}   @/td{1/3} @/td{1/3} @/td{1/3} @/td{0}]
  @/tr[@/th{5} @/td{0}   @/td{0}   @/td{0}   @/td{1/3} @/td{1/3} @/td{1/3}]
  @/tr[@/th{6} @/td{0}   @/td{0}   @/td{0}   @/td{0}   @/td{1/3} @/td{2/3}]
]

Here's the result:

@highlight['python]|{
  >>> Counter(sample1v2() for i in range(1000))
  Counter({1: 188, 2: 173, 5: 168, 6: 166, 3: 164, 4: 141})
  >>> Counter(sample1v2() for i in range(1000))
  Counter({3: 192, 5: 167, 2: 163, 6: 161, 4: 159, 1: 158})
  >>> Counter(sample1v2() for i in range(1000))
  Counter({1: 184, 5: 177, 2: 175, 6: 169, 3: 154, 4: 141})
}|
