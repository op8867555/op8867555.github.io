---
title: '筆記：幾個 CLI 實用工具: ag, fzf, fd, ripgrep'
tags: cli, find, grep, linux
date: 2017-11-27
---

筆記最近用的幾個 CLI 工具。

# The Silver Searcher

[The Silver Searcher][ag] 是一個類似 ack 的搜尋工具，也就是找出出現某字串的所有檔案。

  * 飛天快
  * 預設排除 `.gitignore` `.hgignore` 裡的 pattern
  * 其他不想要的 pattern 可以寫在 `.ignore`
  * 用 c 實作
  * 直到 @caasih 提到之前我都沒發現執行檔名稱 `ag` 是水銀的元素符號


# ripgrep

[Ripgrep][rg] 跟前述的 The Silver Searcher 功能差不多

  * 但是比飛天快還快
  * 對於 ignore 檔案的實作更正確，ag 會多吐一些不應該出現的結果
  * regexp 的功能比較少（比方說我從來沒用過的 lookaround 跟 backreference 就沒有實作）
  * 用 rust 實作

# fd

[fd][fd] 是一個類似 find 的檔案搜尋工具

  * 也會預設排除 `.gitignore`
  * 預設排除隱藏檔案
  * 也是飛天快
  * 也是 rust 實作

## 範例

  * 用 regexp 搜尋

    ~~~~
    fd '[0-9]\.jpg$'
    ~~~~

  * 搜尋隱藏檔案跟被 ignore 的檔案

    ~~~~
    fd -H -I '[0-9]\.jpg$'
    ~~~~

  * 搜尋特定副檔名

    ~~~~
    fd -e jpg
    ~~~~

  * 對搜尋結果下指令**平行執行**

    ~~~~
    fd -e jpg -x convert {} {.}.png
    ~~~~

    variable example
    -------- -------------
    {}       documents/images/party.jpg
    {.}      documents/images/party
    {/}      party.jpg
    {//}     documents/images
    {/.}     party

# fzf

[fzf][fzf] 是一個 fuzzy finder，就是 cmd+T 或是 ctrl+p 或是任何一個 文字編輯器/IDE 裡面不用打全名就可以找檔案的那個功能

  * 用 go 寫的
  * 預設用 find ，所以很慢，用環境變數可以換用 fd

    export FZF_DEFAULT_COMMAND='fd --type f --follow --exclude .git'

  * ctrl+r 來搜尋歷史

    <blockquote class="imgur-embed-pub" lang="en" data-id="IwjCK2l"><a href="//imgur.com/IwjCK2l">View post on imgur.com</a></blockquote><script async src="//s.imgur.com/min/embed.js" charset="utf-8"></script>

  * `**<tab>` 可以選取多個檔案

    <blockquote class="imgur-embed-pub" lang="en" data-id="ocxqb0C"><a href="//imgur.com/ocxqb0C">View post on imgur.com</a></blockquote><script async src="//s.imgur.com/min/embed.js" charset="utf-8"></script>

  * ctrl+t 來搜尋檔案

    <blockquote class="imgur-embed-pub" lang="en" data-id="SfiGWjD"><a href="//imgur.com/SfiGWjD">View post on imgur.com</a></blockquote><script async src="//s.imgur.com/min/embed.js" charset="utf-8"></script>


[ag]: https://github.com/ggreer/the_silver_searcher
[rg]: https://github.com/BurntSushi/ripgrep
[fd]: https://github.com/sharkdp/fd
[fzf]: https://github.com/junegunn/fzf

