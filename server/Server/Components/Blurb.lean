/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
import Server.Jsx

namespace Server.Components
open Server.Jsx

def blurbComponent : Element :=
  <>
    <h2 id="about">About</h2>
    <p>{"Loogle searches of Lean and Mathlib definitions and theorems."}</p>
    <p>
        "You may also want to try the"
        <a href="https://github.com/nomeata/loogle">CLI version</a>
        ", the"
        <a href="https://marketplace.visualstudio.com/items?itemName=ShreyasSrinivas.loogle-lean">VS Code extension</a>
        ", the"
        <a href="https://github.com/Julian/lean.nvim#features"><code>lean.nvim</code> integration</a>
        "or the"
        <a href="https://github.com/nomeata/loogle#zulip-bot">Zulip bot</a>.
    </p>
    <h2 id="usage">Usage</h2>
    <p>Loogle finds definitions and lemmas in various ways:</p>
    <ol type="1">
        <li>
            <p>
                "By constant:"
                <br />
                {"🔍"}
                <a href="/?q=Real.sin"><code>Real.sin</code></a><br />
                "finds all lemmas whose statement somehow mentions the sine function."
            </p>
        </li>
        <li>
            <p>
                By lemma name substring:
                <br />
                {"🔍"}
                <a href="/?q=%22differ%22"><code>"differ"</code></a><br />
                finds all lemmas that have <code>"differ"</code> somewhere in their lemma <em>name</em>.
            </p>
        </li>
        <li>
            <p>
                By subexpression:<br />
                {"🔍"}
                <a href="/?q=_%20*%20(_%20%5E%20_)"><code>_ * (_ ^ _)</code></a><br />
                finds all lemmas whose statements somewhere include a product where the second argument is raised to some power.
            </p>
            <p>
                The pattern can also be non-linear, as in<br />
                {"🔍"}
                <a href="/?q=Real.sqrt%20%3Fa%20*%20Real.sqrt%20%3Fa"><code>Real.sqrt ?a * Real.sqrt ?a</code></a>
            </p>
            <p>
                If the pattern has parameters, they are matched in any order. Both of these will find <code>List.map</code>:<br />
                {"🔍"}
                <a href="/?q=(?a%20-%3E%20?b)%20-%3E%20List%20?a%20-%3E%20List%20?b"><code>(?a -&gt; ?b) -&gt; List ?a -&gt; List ?b</code></a><br />
                {"🔍"}
                <a href="/?q=List%20?a%20-%3E%20(?a%20-%3E%20?b)%20-%3E%20List%20?b"><code>List ?a -&gt; (?a -&gt; ?b) -&gt; List ?b</code></a>
            </p>
        </li>
        <li>
            <p>
                By main conclusion:<br />
                {"🔍"}
                <a href="/?q=%7C-%20tsum%20_%20=%20_%20*%20tsum%20_"><code>|- tsum _ = _ * tsum _</code></a><br />
                finds all lemmas where the conclusion (the subexpression to the right of all <code>→</code> and <code>∀</code>) has the given shape.
            </p>
            <p>
                As before, if the pattern has parameters, they are matched against the hypotheses of the lemma in any order; for example,<br />
                {"🔍"}
                <a href="/?q=%7C-%20_%20%3C%20_%20→%20tsum%20_%20%3C%20tsum%20_"><code>|- _ &lt; _ → tsum _ &lt; tsum _</code></a><br />
                will find <code>tsum_lt_tsum</code> even though the hypothesis <code>f i &lt; g i</code> is not the last.
            </p>
        </li>
    </ol>
    <p>
        If you pass more than one such search filter, separated by commas Loogle will return lemmas which match <em>all</em> of them. The search<br />
        {"🔍"}
        <a href="/?q=Real.sin,%20%22two%22,%20tsum,%20_%20*%20_,%20_%20%5E%20_,%20%7C-%20_%20%3C%20_%20→%20_"><code>Real.sin, "two", tsum, _ * _, _ ^ _, |- _ &lt; _ → _</code></a><br />
        woould find all lemmas which mention the constants <code>Real.sin</code> and <code>tsum</code>, have <code>"two"</code> as a substring of the lemma name, include a product and a power somewhere in the type, <em>and</em> have a
        hypothesis of the form <code>_ &lt; _</code> (if there were any such lemmas). Metavariables (<code>?a</code>) are assigned independently in each filter.
    </p>
    <p>The <code>{"#lucky"}</code> button will directly send you to the documentation of the first hit.</p>
    <h2 id="source-code">Source code</h2>
    <p>
        You can find the source code for this service at <a href="https://github.com/nomeata/loogle" class="uri">https://github.com/nomeata/loogle</a>. The
        <a href="https://loogle.lean-lang.org/" class="uri">https://loogle.lean-lang.org/</a> service is currently provided by Joachim Breitner &lt;<a href="mailto:mail@joachim-breitner.de" class="email">mail@joachim-breitner.de</a>&gt;.
    </p>
  </>
