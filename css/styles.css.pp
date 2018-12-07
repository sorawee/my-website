#lang pollen

body .main-content {
  font-family: "Fira Sans";
}

/* from try-pollen */
.latex-sub, .latex-sup { text-transform: uppercase;
                         font-size: smaller;
                         position: relative; }

.latex-sub { top: 0.2rem;
             margin-left: -0.1667rem;
             margin-right: -0.125rem; }

.latex-sup { top: -0.2rem;
             margin-left: -0.36rem;
             margin-right: -0.15rem;
             text-shadow: none; }

.latex::selection, .latex span:not(.latex-sup)::selection { text-shadow: 0.03em 0 #b4d5fe, -0.03em 0 #b4d5fe, 0 0.03em #b4d5fe, 0 -0.03em #b4d5fe, 0.06em 0 #b4d5fe, -0.06em 0 #b4d5fe, 0.09em 0 #b4d5fe, -0.09em 0 #b4d5fe, 0.12em 0 #b4d5fe, -0.12em 0 #b4d5fe, 0.15em 0 #b4d5fe, -0.15em 0 #b4d5fe;
                    background: #b4d5fe; }

.latex::-moz-selection, .latex span:not(.latex-sup)::-moz-selection { text-shadow: 0.03em 0 #b4d5fe, -0.03em 0 #b4d5fe, 0 0.03em #b4d5fe, 0 -0.03em #b4d5fe, 0.06em 0 #b4d5fe, -0.06em 0 #b4d5fe, 0.09em 0 #b4d5fe, -0.09em 0 #b4d5fe, 0.12em 0 #b4d5fe, -0.12em 0 #b4d5fe, 0.15em 0 #b4d5fe, -0.15em 0 #b4d5fe;
                         background: #b4d5fe; }

.smallcaps {
    text-transform: uppercase;
    font-size: 90%;
    letter-spacing: 1px;
}

.sidenote-number { counter-increment: sidenote-counter; }

.sidenote-number:after, .sidenote:before { content: counter(sidenote-counter) " ";
                                           font-size: 0.8rem;
                                           position: relative;
                                           vertical-align: baseline; }

.sidenote-number:after { content: counter(sidenote-counter);
                         top: -0.5rem;
                         left: 0.1rem; }

.sidenote:before { content: counter(sidenote-counter) " ";
                   top: -0.5rem; }


#the-main {
  width: 100%;
  max-width: 100%;
  padding-left: 2rem;
  padding-right: 2rem;
}

#real-content {
  width: 75%;
}

h1.project-name {
  margin-top: 1rem;
  margin-bottom: 0rem;
}

input.margin-toggle { display: none; }
label.sidenote-number { display: inline; }
label.margin-toggle:not(.sidenote-number) { display: none; }

.sidenote, .marginnote {
  float: right;
  clear: right;
  margin-right: -34%;
  width: 30%;
  margin-top: 0;
  margin-bottom: 20px;
  font-size: 0.95rem;
  line-height: 1.3;
  vertical-align: baseline;
  position: relative;
}

label { cursor: pointer; }

@"@"media (max-width: 760px) {
  #real-content {
    width: 100%;
  }
  label.margin-toggle:not(.sidenote-number) { display: inline; }
  .sidenote, .marginnote { display: none; }
  .margin-toggle:checked + .sidenote,
  .margin-toggle:checked + .marginnote { display: block;
                                         float: left;
                                         left: 1rem;
                                         clear: both;
                                         width: 95%;
                                         margin: 1rem 2.5%;
                                         vertical-align: baseline;
                                         position: relative; }
  label.margin-toggle, label.sidenote-number { color: red; }
}

input.fold-toggle { display: none; }
label.fold-toggle {
  content: "[show content ⊕]";
  display: inline;
  font-weight: 700;
}
label.fold-toggle:hover {
  background-color: #fbf3f3;
}
.folded-content {
  display: none;
}
.fold-toggle:checked + .folded-content {
  background-color: lightyellow;
  padding: 15px;
  margin-top: 15px;
  display: block;
}

@|filename-tag|.@|filename-class| {
  padding-bottom: 0.1rem;
  padding-top: 0.1rem;
  padding-left: 0.5rem;
  border-top-right-radius: 0.5rem;
  background: #f6f6f6;
  font-family: triplicate-t4c, Consolas, Courier;
  font-size: 1rem;
  color: black;
  font-weight: 700;
  position: relative;
  top: 1px;
}

nav {
  width: 100%;
  position: fixed;
  top: 0px;
  left: 0px;
  border: solid #ccc 1px;
  padding: 0; margin: 0;
  background-color: #efefef;
  display: block;
  z-index: 10000;
}

nav ul {
  margin:0; padding: 0;
  width: 100%;
}

nav ul li {
 display: inline-block;
 border-right: solid #ccc 1px;
 padding: 0 5px;
 text-align: center;
 font-size: 80%;
}

.tags {
  font-size: 80%;
}


nav ul li a:link {
  text-decoration: none;
  border: none;
  font-style: italic;
  background: none;
  text-shadow: none;
}
nav ul li a:link img { width: auto; }

.page-header .project-name a {
  color: white;
  text-decoration: none;
}

.main-content table.sourcetable td {
  border: 0;
  padding: 0;
}

.main-content table.sourcetable td.linenos {
  border-right: thin solid grey;
}

.main-content table.sourcetable td.code {
  width: 100%;
}

.main-content table.sourcetable pre {
  border: 0;
  padding: 0.5rem;
}

.main-content li {
  padding-bottom: 10px;
}

.sourcetable {
  background: #f3f6fa;
}

body {
  counter-reset: sidenote-counter;
}

.outdated {
  background-color: peachpuff;
}

.outdated > div:first-child {
  padding-top: 1rem;
  padding-left: 1rem;
  padding-right: 1rem;
}

.outdated > div:last-child {
  padding: 1rem 2rem;
}

.no-bullet {
  list-style: none;
}

.logic-conclusion {
  text-indent: -1em;
}

.main-content hr.logic-separator {
  background-color: darkgray;
  margin-bottom: 1px;
}

.argument {
  padding-right: 1rem;
  padding-top: 1rem;
  background-color: aliceblue;
}

.logic-conclusion:before {
  content: "∴";
  padding-right: 5px;
}

.lemma {
  padding: 1rem;
  margin: 1rem 0;
  border-style: solid;
}

.proof:after {
    content: "\25FC";
    float:right;
}

.subproof {
    padding-right: 1rem;
}

