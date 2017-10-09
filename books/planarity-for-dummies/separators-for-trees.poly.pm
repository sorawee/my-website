#lang pollen

@(define-meta title "Separators for Trees")
@(define-meta tags (planarity))

A separator for a tree is a vertex or edge in a tree whose deletion results in smaller trees. Usually, the separator will be picked so that each smaller tree is guaranteed to have weight@numbered-note{Might be weight of vertices, or edges, or both} at most some factor times that of the original tree.

Formally, let @${\hat{w}(v)} be the total weight of a subtree with root at vertex @${v}. A separator on a tree @${T} is @${\alpha}-balanced if it is guaranteed that, after the deletion, each smaller tree $t_i$ has weight @${\hat{w}(t_i) \le \alpha \hat{w}(T)}, for @${0 < \alpha < 1}. If possible, we would want @${\alpha} to be close to 0.5, as that means the largest subtree is guaranteed to be small. This is very useful for divide and conquer algorithms, as it allows one to write an algorithm with running time like @${T(n) = T(\alpha n) + T((1 - \alpha) n) + O(n)}, for example.

There are several kind of separators for trees. An edge separator would split the tree to exactly two subtrees, while there is no limit for a vertex separator.

@figure["/books/planarity-for-dummies/tree-separator-1.png" #:caption
        @ids{Figure 1: A @${\frac{1}{2}}-balanced vertex separator}]

Before talking about a specific separator, we need a general algorithm to find a "heavy vertex" on a @emph{rooted} tree @${T} with weight on only vertices (and not edges) which is a greedy way to find an @${\alpha}-balanced point quickly. This will be the core of the algorithm for finding separators:

@lemma{
  (Leafmost Heavy Vertex) The linear time algorithm below finds a vertex @${z} such that @${\hat{w}(z) > \alpha \hat{w}(T)} and for each child @${t_i} of @${z}, @${\hat{w}(t_i) \le \alpha \hat{w}(T)}

  @pre[#:options '(allowed-math)]{
    def heavy-vertex(@${T}, @${\alpha}):
      def f(@${v}):
        if there is a child @${u} of @${v} where @${\hat{w}(u) > \alpha \hat{w}(T)}:
          return f(@${u})
        else:
          return @${v}
      return f(root(@${T}))
  }

  @proof{
    Let @${z} be the vertex that the algorithm returns. Let @${t_1, t_2, ..., t_k} be all children of @${z}. we know that @${\hat{w}(t_i) \le \alpha \hat{w}(T)} as otherwise, the algorithm would continue recurring and not return @${z}. Also, @${\hat{w}(z) > \alpha \hat{w}(T)} because @${f(z)} is called only when @${\hat{w}(z) > \alpha \hat{w}(T)}. This concludes the lemma as desired.
  }
}

The figure below illustrates the algorithm. When @${f} is called on ancestors of @${z}, it finds that a subtree (which contains @${z}) has weight greater than @${\alpha \hat{w}(T)}, so it continues recursion. When @${f} is called on @${z},  @${\hat{w}(z) > \alpha \hat{w}(T)} but @${\hat{w}(t_i) \le \alpha \hat{w}(T)}, so the algorithm returns @${z}.

@figure["/books/planarity-for-dummies/tree-separator-2.png" #:width "30%" #:caption
       @ids{Figure 2: Heavy vertex @${z} is a @${\frac{1}{2}}-balanced vertex separator}]

@subsection{Vertex Separator of Vertex Weight}

@lemma{
  (Tree Vertex Separator of Vertex Weight) If a tree @${T} has weight on only vertices and not edges, there is a linear time algorithm to find a @${\frac{1}{2}}-balanced vertex separator.

  @proof{
    Transform @${T} into a rooted tree by picking an arbitrary vertex as the root. Now, run heavy-vertex(@${T}, @${\frac{1}{2}}). Let @${z} be the returned vertex. The Leafmost Heavy Vertex Lemma states that the blue subtree with root @${z} in Figure 2 has weight @${\hat{w}(z) > \frac{1}{2} \hat{w}(T)}. For each child @${t_i} of @${z}, @${\hat{w}(t_i) \le \frac{1}{2} \hat{w}(T)}. Now, the red subtree @${t_0} rooted at the parent of @${z} after its deletion, which is @${y}, would contain the weight of the whole tree excluding the weight of the blue subtree rooted at @${z}. In other words, the weight is @${\hat{w}(T) - \hat{w}(z)}. However, because @${\hat{w}(z) > \frac{1}{2} \hat{w}(T)}, it follows that @${\hat{w}(T) - \hat{w}(z) < \frac{1}{2} \hat{w}(T)}. This shows that every subtree @${t_0, t_1, t_2, ..., t_k} has weight at most @${\frac{1}{2} \hat{w}(T)}, meaning that the separator @${z} is @${\frac{1}{2}}-balanced.
  }
}
