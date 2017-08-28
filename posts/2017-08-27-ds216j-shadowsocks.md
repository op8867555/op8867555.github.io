---
title: 筆記： DS216j 安裝 shadowsocks server
tags: shadowsocks, openwrt, nas
---

最近親戚弄了台 Synology DS216j 想拿來架 shadowsocks 的 server，這篇文章筆記我架這個 server 所走的路。

# 折騰過程

提到在 NAS 上架 shadowsocks，可以查到很多用 docker 的解決方法，不過網路上的幾篇文章都說這台機器不能用 docker，所以我直接往 binary 的方法前進。

一開始看到的是這篇 [Synology DS216Play 安装 ShadowSocks](http://jexbat.com/2016/NAS-Shadowsocks/)，從這篇文章中得出：

  * 原來有 package manager 可以用
  * shadowsocks 有用 python 跟 libev 的實作可以選

## 安裝package manager

朝著安裝 package manager 的方向前進後找到了這篇[Synology DS216j Optware IPKG 介紹][]。  
看了看發現這是篇翻譯文，而原文有了更新的[版本🇯🇵][Synology DS216j Entware-ng 導入]，發現到有 [entware-ng][] 這個比 ipkg 更新的 package manager 能用，
而且已經有 shadowsocks 的[套件能用](pkg.entware.net/binaries/armv7/Packages.html)。

## 安裝 entware-ng

安裝的過程不難，大致上就是開好SSH、照著[其 wiki 上寫的步驟][Install on Synology NAS]就行。

筆記一下步驟：

  * 建立一個套件管理用的目錄，因為韌體更新會清 /opt ，所以要在外面建並 symlink 回去：

    ~~~~bash
    mkdir -p /volume1/@entware-ng/opt
    rm -rf /opt
    ln -sf /volume1/@entware-ng/opt /opt
    ~~~~

  * 安裝 entware

    ~~~~bash
    wget -O - http://pkg.entware.net/binaries/armv7/installer/entware_install.sh | /bin/sh
    ~~~~

  * 設定啟動腳本（重開機時把 entware 裝的服務也帶起來）  
    編輯 /usr/local/etc/rc.d/entware-startup.sh （教學上沒有，但是我有順便做 `chmod +x`）

    ~~~~bash
    #!/bin/sh

    case $1 in
        start)
        mkdir -p /opt
        mount -o bind /volume1/@entware-ng/opt /opt
        /opt/etc/init.d/rc.unslung start
        ;;
        stop)
        ;;
    esac
    ~~~~

  * 在登入時自動把 `/opt/bin` `/opt/sbin`加到 `PATH` 裡面：在 /etc/profile 內加上一行

        . /opt/etc/profile

裝完之後就是開心地來裝 shadowsocks

    opkg update
    opkg install shadowsocks-libev

但是裝完卻發現沒有 `ss-server` 這個執行檔！
原因似乎是因為 entware-ng 沒有跟上流的 openwrt 同步 [^entware-ng-e3793bbb]，而不知為何的明明就有新的 Makefile ，除了 mipsel 以外卻沒有新的 binary package 能用。


## 手動編譯

沒辦法，只好來自己編。幸好 entware 的 wiki 頁上也有[教學][Compile packages from sources]，也是照著做就好。

Entware-ng 用的是 OpenWrt Buildroot，所以需要先裝好[它依賴的套件][OpenWrt build system – Installation]。

為了不把主力機的 Arch Linux 給搞髒了，我決定用找 docker 的方式來解決。  
網路上可以找到一個用 arch 為底的 Dockerfile([arch-docker-buildroot][])，但是實際編譯時會出先一些版本上的不相容問題，所以我還是乖乖的用 ubuntu 的 image 來做。

~~~~dockerfile
FROM ubuntu
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential subversion libncurses5-dev zlib1g-dev gawk gcc-multilib flex git-core gettext libssl-dev unzip python-dev python file wget sudo

# 編譯時需要非 root 帳號
RUN useradd -m openwrt &&\
echo 'openwrt ALL=NOPASSWD: ALL' > /etc/sudoers.d/openwrt
USER openwrt
WORKDIR /home/openwrt
CMD ["/bin/bash"]
~~~~

docker 環境準備好、進入其 shell 之後

  * 準備 entware 跟更新其 package feeds：

    ~~~~{.bash .example}
    git clone https://github.com/Entware-ng/Entware-ng.git
    cd Entware-ng
    make package/symlinks
    cp configs/armv7.config .config
    ~~~~

  * 安裝編譯需要 tools/toolchain，這一步驟需要很長一段時間，我就跑去打 Splatoon 的 Salmon Run 了。

    ~~~~{.bash .example}
    make -j4 tools/install
    make -j4 toolchain/install
    ~~~~

  * **確認 .config 裡面有啟用 `shadowsocks-libev` 跟 `shadowsocks-libev-ss-server` 兩個套件**，
    我在編的時候沒確認到這點，花了好一段時間研究為什麼編完沒有 binary 檔。

    ~~~~{.bash .example}
    make menuconfig # 選好 shadowsocks （Network 的 Web Servers/Proxies 底下）
    ~~~~

    或是直接在 .config 加上

    ~~~~{.bash .example}
    CONFIG_PACKAGE_shadowsocks-libev-config=m
    CONFIG_PACKAGE_shadowsocks-libev-ss-server=m
    ~~~~

  * 然後編譯需要的套件

    ~~~~{.bash .example}
    make -j4 package/shadowsocks-libev/compile
    ~~~~

編譯完之後會在 bin 下面找到對應的 .ipkg 檔，接著就是傳到 NAS 上

  * 用 opkg 安裝：

        opkg install shadowsocks-libev-config_3.0.6-2_armv7soft.ipk
        opkg install shadowsocks-libev-ss-server_3.0.6-2_armv7soft.ipk

  * 設定好 `/opt/etc/shadowsocks.json` 跟 `/opt/etc/init.d/S22shadowsocks`：

        sed -ir 's/PROCS=ss-local/PROCS=ss-server/' /opt/etc/init.d/S22shadowsocks

  * 確認 shadowsocks-libev 可以啟動

        /opt/etc/init.d/S22shadowsocks start

設定好 router 的 NAT，然後用手機之類的測試會動之後，收工。




[OpenWrt build system – Installation]: http://wiki.openwrt.org/doc/howto/buildroot.exigence#install_procedure_on_linux

[arch-docker-buildroot]: https://github.com/jannispinter/arch-openwrt-buildroot
[Compile packages from sources]: https://github.com/Entware-ng/Entware-ng/wiki/Compile-packages-from-sources

[entware-ng#701]: https://github.com/Entware-ng/Entware-ng/issues/701
[^entware-ng-e3793bbb]: <https://github.com/Entware-ng/entware-packages/commit/e3793bbbde8b907842f84731bfec292ccb069114>



[Synology DS216j Optware IPKG 介紹]: https://ky0n.xyz/synology-ds216j-optware-ipkg-init/
[Synology DS216j Entware-ng 導入]: http://jasmin.sakura.ne.jp/blog/0245
[Install on Synology NAS]: https://github.com/Entware-ng/Entware-ng/wiki/Install-on-Synology-NAS
[entware-ng]: https://github.com/Entware-ng/Entware-ng/
