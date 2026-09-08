class: center, middle

# (Functional) programming in the AI age 🤖

## Lesson 6

<img src="img/logo_wordmark_black.png" height="96">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![TN logo](img/tn.png)

<br/>

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

---

# Let AI do its thing: have no leash

* AI should run in a safe environment where it cannot break things
* If you're approving permissions: use `grep`? Yes, read `foo.hs`? Yes
  * You're doing it wrong

Solutions:

* Use a throwaway cloud machine ☁️
* Use a devcontainer 🐋
  * No access to your host
  * Can expose the devcontainer's processes on the host (web server)

> Then you remove permission-asking

---

# AI needs to experiment


