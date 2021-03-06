---
title: ADAM CASA Hub A01 在 Linux 使用狀況＆個人心得
date: 2017/07/20
tags: linux, hardware
image: http://i.imgur.com/Kc6ECEAh.jpg
---

# TL;DR

我買了一個 ADAM CASA Hub A01 ，用來搭配 X1C5 使用，環境是 Arch Linux + Kernel 4.11，我測試了下列項目都正常運作：

  * 1080p 外接螢幕
  * USB 鍵盤、滑鼠
  * USB 3 外接 HDD
  * Type C Power Delivery 充電
  * 網路孔

# 使用心得

上個月買了新筆電，因為 USB 孔比之前的少，也少了網路孔，HDMI 的位置也落在比較不方便的左手邊，就一直想找款 USB-c hub 來當成 dock 用。
可能是因為 usb c 相對還算新玩意兒，網路上找不太到這類產品的資源。
找了一圈之後，最後決定訂了這隻台廠的 hub，主要的原因是：

  * 滿足我的需求：
    * 不能是專為蘋果設計的 *(yuck)*
    * 至少一個 USB port
    * 網路孔
    * HDMI 輸出
    * 可以的話有 type c 的 PowerDelivery

  * 官方網站有寫**支援 Linux**  
    身為一個 linux user ，這點的加分實在很大。其他類似的產品都只寫支援 mac / chromebook。

## 批評

這邊來談談我個人體感的幾個缺點

  * **線的長度太短**  
    這個產品設計應該沒有考慮到平放於桌上以外的狀況，
    我只是將筆電墊高了約 5cm，就可以明顯感受到這個問題。

    ![](http://i.imgur.com/Kc6ECEA.jpg)

    我得再花個幾百 NTD 買條延長線或是想辦法架高 hub 來解決這個問題。

  * **開口方向設計不實用**  
    當像我一樣，usb c port 在筆電左方時，有幾個擺放方式：

      a) 橫著正放（開口向正面）： HDMI 跟 PD 面對正面 ⇒ NO
      b) 橫著倒放（開口向背面）： SD 卡 ⇒ NO
      c) 直著放（開口向左）： 網路孔面對正面 ⇒ NO

    沒有一個辦法是完美的，不過市面上的其他場品也有類似的問題。

# 一些 log

## USB 裝置

`lsusb` 會多出下列裝置

    Bus 004 Device 003: ID 0bda:8153 Realtek Semiconductor Corp. RTL8153 Gigabit Ethernet Adapter
    Bus 004 Device 002: ID 2109:0813 VIA Labs, Inc. 
    Bus 004 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
    Bus 003 Device 008: ID 2109:0100 VIA Labs, Inc. 
    Bus 003 Device 006: ID 05e3:0752 Genesys Logic, Inc. 
    Bus 003 Device 004: ID 05e3:0618 Genesys Logic, Inc. 
    Bus 003 Device 002: ID 2109:2813 VIA Labs, Inc. 
    Bus 003 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub


## xRandr

  * HDMI 接上螢幕之後，會在接在 DP1 或是 DP2 輸出。
  * EDID 跟原本的一樣不變 (會 passthrough)。

~~~~{.half-page}
Screen 0: minimum 8 x 8, current 3000 x 1920, maximum 32767 x 32767
DP1 disconnected (normal left inverted right x axis y axis)
DP2 connected 1080x1920+1920+0 right (normal left inverted right x axis y axis) 480mm x 270mm
   1920x1080     60.00*+  50.00    59.94  
   1920x1080i    60.00    50.00    59.94  
   1680x1050     59.88  
   1600x900      60.00  
   1280x1024     60.02  
   1280x800      59.91  
   1280x720      60.00    50.00    59.94  
   1024x768      60.00  
   800x600       60.32  
   720x576       50.00  
   720x480       60.00    59.94  
   640x480       60.00    59.94  
HDMI1 disconnected (normal left inverted right x axis y axis)
VIRTUAL1 disconnected (normal left inverted right x axis y axis)
~~~~

## Dmesg

~~~~{.half-page}
[388483.356326] ucsi_acpi USBC000:00: ucsi_connector_change: failed to read connector status (-110)
[388485.016551] pci 0000:06:00.0: [8086:15d3] type 01 class 0x060400
[388485.016800] pci 0000:06:00.0: supports D1 D2
[388485.016803] pci 0000:06:00.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.017256] pci 0000:07:00.0: [8086:15d3] type 01 class 0x060400
[388485.017402] pci 0000:07:00.0: supports D1 D2
[388485.017404] pci 0000:07:00.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.017525] pci 0000:07:01.0: [8086:15d3] type 01 class 0x060400
[388485.017665] pci 0000:07:01.0: supports D1 D2
[388485.017667] pci 0000:07:01.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.017788] pci 0000:07:02.0: [8086:15d3] type 01 class 0x060400
[388485.017929] pci 0000:07:02.0: supports D1 D2
[388485.017930] pci 0000:07:02.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.018053] pci 0000:07:04.0: [8086:15d3] type 01 class 0x060400
[388485.018194] pci 0000:07:04.0: supports D1 D2
[388485.018196] pci 0000:07:04.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.018336] pci 0000:06:00.0: PCI bridge to [bus 07-70]
[388485.018348] pci 0000:06:00.0:   bridge window [mem 0xbc000000-0xea0fffff]
[388485.018357] pci 0000:06:00.0:   bridge window [mem 0x70000000-0xb9ffffff 64bit pref]
[388485.018431] pci 0000:07:00.0: PCI bridge to [bus 08]
[388485.018442] pci 0000:07:00.0:   bridge window [mem 0xea000000-0xea0fffff]
[388485.018515] pci 0000:07:01.0: PCI bridge to [bus 09-3b]
[388485.018526] pci 0000:07:01.0:   bridge window [mem 0xbc000000-0xd3efffff]
[388485.018534] pci 0000:07:01.0:   bridge window [mem 0x70000000-0x8fffffff 64bit pref]
[388485.018637] pci 0000:3c:00.0: [8086:15d4] type 00 class 0x0c0330
[388485.018663] pci 0000:3c:00.0: reg 0x10: [mem 0xd3f00000-0xd3f0ffff]
[388485.018858] pci 0000:3c:00.0: supports D1 D2
[388485.018860] pci 0000:3c:00.0: PME# supported from D0 D1 D2 D3hot D3cold
[388485.019054] pci 0000:07:02.0: PCI bridge to [bus 3c]
[388485.019066] pci 0000:07:02.0:   bridge window [mem 0xd3f00000-0xd3ffffff]
[388485.019141] pci 0000:07:04.0: PCI bridge to [bus 3d-70]
[388485.019152] pci 0000:07:04.0:   bridge window [mem 0xd4000000-0xe9ffffff]
[388485.019160] pci 0000:07:04.0:   bridge window [mem 0x90000000-0xb9ffffff 64bit pref]
[388485.019194] pci_bus 0000:07: Allocating resources
[388485.019235] pci 0000:07:01.0: bridge window [io  0x1000-0x0fff] to [bus 09-3b] add_size 1000
[388485.019246] pci 0000:07:02.0: bridge window [io  0x1000-0x0fff] to [bus 3c] add_size 1000
[388485.019249] pci 0000:07:02.0: bridge window [mem 0x00100000-0x000fffff 64bit pref] to [bus 3c] add_size 200000 add_align 100000
[388485.019260] pci 0000:07:04.0: bridge window [io  0x1000-0x0fff] to [bus 3d-70] add_size 1000
[388485.019272] pci 0000:06:00.0: bridge window [io  0x1000-0x0fff] to [bus 07-70] add_size 3000
[388485.019277] pci 0000:06:00.0: BAR 13: no space for [io  size 0x3000]
[388485.019279] pci 0000:06:00.0: BAR 13: failed to assign [io  size 0x3000]
[388485.019283] pci 0000:06:00.0: BAR 13: no space for [io  size 0x3000]
[388485.019285] pci 0000:06:00.0: BAR 13: failed to assign [io  size 0x3000]
[388485.019293] pci 0000:07:02.0: BAR 15: no space for [mem size 0x00200000 64bit pref]
[388485.019295] pci 0000:07:02.0: BAR 15: failed to assign [mem size 0x00200000 64bit pref]
[388485.019297] pci 0000:07:01.0: BAR 13: no space for [io  size 0x1000]
[388485.019298] pci 0000:07:01.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019301] pci 0000:07:02.0: BAR 13: no space for [io  size 0x1000]
[388485.019302] pci 0000:07:02.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019304] pci 0000:07:04.0: BAR 13: no space for [io  size 0x1000]
[388485.019306] pci 0000:07:04.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019309] pci 0000:07:04.0: BAR 13: no space for [io  size 0x1000]
[388485.019311] pci 0000:07:04.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019315] pci 0000:07:02.0: BAR 15: no space for [mem size 0x00200000 64bit pref]
[388485.019317] pci 0000:07:02.0: BAR 15: failed to assign [mem size 0x00200000 64bit pref]
[388485.019318] pci 0000:07:02.0: BAR 13: no space for [io  size 0x1000]
[388485.019320] pci 0000:07:02.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019322] pci 0000:07:01.0: BAR 13: no space for [io  size 0x1000]
[388485.019324] pci 0000:07:01.0: BAR 13: failed to assign [io  size 0x1000]
[388485.019327] pci 0000:07:00.0: PCI bridge to [bus 08]
[388485.019334] pci 0000:07:00.0:   bridge window [mem 0xea000000-0xea0fffff]
[388485.019369] pci 0000:07:01.0: PCI bridge to [bus 09-3b]
[388485.019375] pci 0000:07:01.0:   bridge window [mem 0xbc000000-0xd3efffff]
[388485.019380] pci 0000:07:01.0:   bridge window [mem 0x70000000-0x8fffffff 64bit pref]
[388485.019389] pci 0000:07:02.0: PCI bridge to [bus 3c]
[388485.019395] pci 0000:07:02.0:   bridge window [mem 0xd3f00000-0xd3ffffff]
[388485.019404] pci 0000:07:04.0: PCI bridge to [bus 3d-70]
[388485.019409] pci 0000:07:04.0:   bridge window [mem 0xd4000000-0xe9ffffff]
[388485.019413] pci 0000:07:04.0:   bridge window [mem 0x90000000-0xb9ffffff 64bit pref]
[388485.019420] pci 0000:06:00.0: PCI bridge to [bus 07-70]
[388485.019425] pci 0000:06:00.0:   bridge window [mem 0xbc000000-0xea0fffff]
[388485.019429] pci 0000:06:00.0:   bridge window [mem 0x70000000-0xb9ffffff 64bit pref]
[388485.021125] xhci_hcd 0000:3c:00.0: xHCI Host Controller
[388485.021134] xhci_hcd 0000:3c:00.0: new USB bus registered, assigned bus number 3
[388485.022313] xhci_hcd 0000:3c:00.0: hcc params 0x200077c1 hci version 0x110 quirks 0x00009810
[388485.024330] hub 3-0:1.0: USB hub found
[388485.024341] hub 3-0:1.0: 2 ports detected
[388485.024949] xhci_hcd 0000:3c:00.0: xHCI Host Controller
[388485.024955] xhci_hcd 0000:3c:00.0: new USB bus registered, assigned bus number 4
[388485.025136] hub 4-0:1.0: USB hub found
[388485.025146] hub 4-0:1.0: 2 ports detected
[388485.342798] usb 3-1: new high-speed USB device number 2 using xhci_hcd
[388485.480782] hub 3-1:1.0: USB hub found
[388485.481599] hub 3-1:1.0: 4 ports detected
[388485.627543] usb 4-1: new SuperSpeed USB device number 2 using xhci_hcd
[388485.730586] hub 4-1:1.0: USB hub found
[388485.730871] hub 4-1:1.0: 4 ports detected
[388485.812902] usb 3-1.3: new high-speed USB device number 3 using xhci_hcd
[388485.922630] hub 3-1.3:1.0: USB hub found
[388485.922928] hub 3-1.3:1.0: 4 ports detected
[388486.112908] usb 4-1.4: new SuperSpeed USB device number 3 using xhci_hcd
[388486.202794] usb 3-1.3.1: new high-speed USB device number 4 using xhci_hcd
[388486.298596] usb-storage 3-1.3.1:1.0: USB Mass Storage device detected
[388486.298988] scsi host0: usb-storage 3-1.3.1:1.0
[388486.366550] usb 4-1.4: reset SuperSpeed USB device number 3 using xhci_hcd
[388486.440411] r8152 4-1.4:1.0 eth0: v1.08.9
[388486.772835] usb 3-1.3.3: new high-speed USB device number 5 using xhci_hcd
[388486.989538] r8152 4-1.4:1.0 enp60s0u1u4: renamed from eth0
[388487.321672] scsi 0:0:0:0: Direct-Access     Generic  STORAGE DEVICE   0233 PQ: 0 ANSI: 0
[388487.325495] sd 0:0:0:0: [sda] Attached SCSI removable disk
~~~~

