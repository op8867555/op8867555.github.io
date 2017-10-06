---
title: 應用 git bisect 找出 linux kernel 中出現問題的 commit
tags: linux, arch, drm-tip, git
---

### TL;DR

這篇文章筆記我使用 git bisect 來找出 linux kernel (drm/i915) 中的導致行為異常的是哪一個 commit 的過程。


# 起因

我的筆電上裝的是 Arch Linux，Chromium 在 linux 上是[不會有硬體加速功能][chromium-issue-hwacc]的，為了減少 CPU 資源的浪費，也為了增加續航力，我使用的是一個非官方版本的 [chromium-vaapi][]。

最近做完系統更新之後發現硬體加速的功能出了些異常，在跟 AUR package 的使用者討論後得知：

  * mesa 的版本會影響，但是降版之後有不同的異常
  * 這個不同的異常只有在播放 VP9 格式的影片時會發生
  * 在一番亂試之後發現到 kernel 降回 4.12 就沒事了！

做了一些功課之後，我認為這個 bug 是來自於 linux kernel 中 DRM/i915 的部份，於是我便開始嘗試從茫茫 commit 海撈出那個針。

[chromium-issue-hwacc]: https://bugs.chromium.org/p/chromium/issues/detail?id=463440
[chromium-vaapi]: https://aur.archlinux.org/packages/chromium-vaapi/

# 找出問題之旅

## drm-tip

drm-tip 是裡面包含著不同的幾個團隊所維護的 drm 實作的 linux kernel

![這個 merge flow 很複雜，我也還沒搞懂](https://01.org/linuxgraphics/gfx-docs/maintainer-tools/_images/drm-intel-flow.svg)

## 在 ArchLinux 上編譯

幸好我不必了解他們是怎麼 merge，只需要 build 起來能測試功能是否正常即可。

  * 使用 AUR 上面的 [linux-drm-tip-git][] 這個 package 編譯最新的版本
  * 遇到 `make prepare` 詢問參數是因為 `config.x86-64` 的版本不夠新
    * 我直接找了 [linux-mainline][] 的來用
  * 先用 `makepkg -Asi` 編譯安裝一遍。
  * 然後使用這個版本的 kernel 後發現、恩、問題依舊

[linux-drm-tip-git]: https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=linux-drm-tip-git&id=f2f3487589a22ee728472259f4bd79af75bd782b
[linux-mainline]: https://aur.archlinux.org/packages/linux-mainline/

## 加快編譯速度

我用了兩個工具來加速編譯速度：

  * [ccache][]: 幫 gcc 做快取，改善重複編譯所需的時間
  * [modprobed-db][]: 減少所需編譯的模組數

[ccache]: https://wiki.archlinux.org/index.php/ccache
[modprobed-db]: https://wiki.archlinux.org/index.php/Modprobed-db

## bisect

要從至少 27000 筆的 commit 中找錯誤，當然不會一筆一筆嘗試。
這邊使用的 bisect 就是用經典的演算法「二分搜尋法」來加快尋找的速度。

### 步驟

  * 編譯完之後，進 `src/drm-tip` 執行 `git bisect start`
  * 確認問題還在，標記為有問題的版本 `git bisect bad`
  * `git checkout` 到認為沒問題的版本（因為我知道 4.12 沒有問題，所以我找到對應的 commit 是`6f7da29`）
  * `makepkg -esfi` 編譯並安裝此版本
    * 因為 `git bisect` 會 checkout 到不同的版本，所以需要用 `-e` 來告訴 makepkg 不要重新 checkout
  * 重開機檢查問題是否還在，沒有問題，用 `git bisect good` 標記

    ~~~~shell
    > git bisect good
    Bisecting: 13952 revisions left to test after this (roughly 14 steps)
    [73adb8c5b0ea15e3942ab5781eb3e72d4e028ada] Merge tag 'for-4.13/dm-fixes-2' of git://git.kernel.org/pub/scm/linux/kernel/git/device-mapper/linux-dm
    ~~~~

### 預估還要幾次查詢

  * `git rev-list drm-tip ^6f7da290413b --count` 告訴我這兩個中間差了 27906 個 commit
  * 所以我預期最多還需要 16 次的篩選 ($log_2(27906)+1 \approx 15.77$)


### 繼續尋找問題

這邊就是不斷的重複：

  * 編譯
  * 測試
  * `git bisect [good|bad]`

~~~~{.half-page}
> git bisect bad 73adb8c5b0ea
Bisecting: 6975 revisions left to test after this (roughly 13 steps)
[4f5dfdd29065a0d1d0e61d9744e14d1d852518be] Merge tag 'leds_for_4.13' of git://git.kernel.org/pub/scm/linux/kernel/git/j.anaszewski/linux-leds

> git bisect good 4f5dfdd29065
Bisecting: 3487 revisions left to test after this (roughly 12 steps)
[8c03cc85a035ae7a208c28c4382ecfeb6adf79a6] fs/proc/task_mmu.c: remove obsolete comment in show_map_vma()

> git bisect bad 8c03cc85a035
Bisecting: 1723 revisions left to test after this (roughly 11 steps)
[2ceedf97aef41d071d897a6e6aec8c05fb707ec4] Merge tag 'dmaengine-4.13-rc1' of git://git.infradead.org/users/vkoul/slave-dma

> git bisect good 2ceedf97aef4
Bisecting: 818 revisions left to test after this (roughly 10 steps)
[04d4fb5fa63876d8e7cf67f2788aecfafc6a28a7] Merge branch 'drm-next-4.13' of git://people.freedesktop.org/~agd5f/linux into drm-next

> git bisect good 04d4fb5fa638
Bisecting: 420 revisions left to test after this (roughly 9 steps)
[00fc2c26bc46a64545cdf95a1511461ea9acecb4] drm: Remove unused drm_file parameter to drm_syncobj_replace_fence()

> git bisect bad 00fc2c26bc46
Bisecting: 222 revisions left to test after this (roughly 8 steps)
[eafae133e48c9e5f5537d5c6df34eab912336b9a] Merge tag 'drm-msm-next-2017-06-20' of git://people.freedesktop.org/~robclark/linux into drm-next

> git bisect good eafae133e48c
Bisecting: 111 revisions left to test after this (roughly 7 steps)
[fc59921178fd63f1dbe445c2fc86e6ca997a4744] drm/i915/perf: Add more OA configs for BDW, CHV, SKL + BXT

> git bisect good fc59921178fd
Bisecting: 55 revisions left to test after this (roughly 6 steps)
[7dd4f6729f9243bd7046c6f04c107a456bda38eb] drm/i915: Async GPU relocation processing

> git bisect bad 7dd4f6729f92
Bisecting: 34 revisions left to test after this (roughly 5 steps)
[615c16a9d8649b9894592d11bc393e684b11e2ea] drm/i915/gvt: Refine virtual reset function

> git bisect good 615c16a9d864
Bisecting: 17 revisions left to test after this (roughly 4 steps)
[f6262bda462e81e959b80a96dac799bd9df27f73] drm/i915: Don't enable backlight at setup time.

> git bisect good f6262bda462e
Bisecting: 8 revisions left to test after this (roughly 3 steps)
[4c9c0d09741deab0aac76b83961cfe95b24f3e6f] drm/i915: Fix retrieval of hangcheck stats

> git bisect good 4c9c0d09741d
Bisecting: 4 revisions left to test after this (roughly 2 steps)
[2889caa9232109afc8881f29a2205abeb5709d0c] drm/i915: Eliminate lots of iterations over the execobjects array

> git bisect bad 2889caa92321
Bisecting: 1 revision left to test after this (roughly 1 step)
[507d977ff965682a925ffe479c95136680fcb77b] drm/i915: Pass vma to relocate entry

> git bisect good 507d977ff965
Bisecting: 0 revisions left to test after this (roughly 0 steps)
[071750e550af46b5d3a84ad56c2a108c3e136284] drm/i915: Disable EXEC_OBJECT_ASYNC when doing relocations
~~~~

### 找出有問題的 commit

不斷重複上面的步驟，最後找出第一個有問題的 commit

~~~~
> git bisect good 071750e550af
2889caa9232109afc8881f29a2205abeb5709d0c is the first bad commit
~~~~


# 後記

這個過程並不是很輕鬆（編譯一次至少需要 10min，還要重開機測試），將這個找到的這個 commit 回報上去之後，開發者很快的就能找到問題的關鍵點。

最後得到的結論是，這個問題應該是來自 user space driver 沒有實作正確，於是我回去找了 intel-vaapi-driver 的 issue tracker，
才發現到原來早在 3 個星期前就有人[回報][vaapi-bug]過，而且也已經修正並 merge 回 master 了。囧

雖然最後發現不是 linux kernel 的問題，不過這也依然是個務實的除錯經驗。

[vaapi-bug]: https://github.com/01org/intel-vaapi-driver/issues/262


# 參考資料

**drm/i915**

  * [how to report bugs](https://01.org/linuxgraphics/documentation/how-report-bugs)
  * [drm-intel](https://01.org/linuxgraphics/gfx-docs/maintainer-tools/drm-intel.html)

**git bisect**

  * [Bisecting bugs](https://wiki.archlinux.org/index.php/Bisecting_bugs)
  * [How I fixed a kernel regression using git bisect](https://project-insanity.org/blog/2013/02/13/how-i-found-a-kernel-regression-using-git-bisect/)



