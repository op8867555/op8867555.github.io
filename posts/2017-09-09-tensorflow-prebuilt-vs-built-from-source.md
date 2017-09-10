---
title: 手動編譯 vs 編譯好的 tensorflow
tags: tensorflow
---


亂看文章時發現到 tensorflow prebuilt 版跟自己 build 效能有落差（現在想想這也是理所當然的事），於是就來自己編編看到底差多少了。

# 實驗

## 使用的機器

Thinkpad X1 Carbon 2017，它的 CPU 是 Intel 的 i7-7500U（對，這顆絕對不適合拿來算這個）

## 使用的不同版本

  * [PyPI 上的版本][pip]，沒有機器相關的最佳化。

  * [intel 提供的版本][intel] 使用 intel 的 Math Kernel library，但是沒有機器相關的最佳化[^opt]。

  * 自己 build ，用的是 tensorflow 官方 github repo 的 [32ffc5a81][github]

    * `./configure` 都用預設值 (`-march=native` 開啟當下機器架構的最佳化）
    *  `--config=mkl` 同 intel 版本


[pip]: https://pypi.python.org/pypi/tensorflow
[intel]: https://software.intel.com/en-us/articles/tensorflow-optimizations-on-modern-intel-architecture
[github]: https://github.com/tensorflow/tensorflow/tree/32ffc5a81eee8c39bbe71536212a773b1ffd4eb2

## 使用的測試案例

使用了 keras 的幾個 [example][keras examples] 來測試[^why-keras]，分別是：

  * mnist-mlp
  * mnist-cnn
  * cifar10-cnn

[keras examples]: https://github.com/fchollet/keras/tree/master/examples
[^why-keras]: 問我為什麼要用 keras 而不用 tensorflow 就好？因為我還想測 theano 啊…

## 測試結果

testcase            pip   intel   built
---------------  ------  ------  ------
mnist-mlp          9.5s   10.5s   10.5s
mnist-cnn        175.5s     63s     60s
cifar10-cnn[^1]    275s    200s    200s
---------------------------------------
: 實驗結果（數值是每次 epoch 所花的時間，越小越好）

[^1]: 因為有 200 個 epoch 要跑，實在要花很多時間，所以這邊就都只取大概 3~4 個 epoch 的時間來算

## 結論

  * 如果是真心想要用 Intel 的 CPU 跑 tensorflow …
    * …也願意自己花時間編譯，那**手動編譯**是（執行時）最快的選項
    * …卻不想自己編，但可以接受不是最新的版本，那直接使用[**intel 提供的版本**][intel]
  * 如果沒那麼真心想用，只是先用 CPU 玩玩看，那[**直接用 pip 裝**][pip]是最簡易的選項。
  * *真心想跑 tensorflow 還是去弄個有 GPU 的環境吧…*
    * 用了 GTX 1060 6GB 的桌電跑 cifar10-cnn ，一個 epoch 只要 25s

[^opt]: 沒有做機器相關的最佳化時會有提醒

    ~~~~
    The TensorFlow library wasn't compiled to use SSE4.1 instructions, but these are available on your machine and could speed up CPU computations.
    The TensorFlow library wasn't compiled to use SSE4.2 instructions, but these are available on your machine and could speed up CPU computations.
    The TensorFlow library wasn't compiled to use AVX instructions, but these are available on your machine and could speed up CPU computations.
    The TensorFlow library wasn't compiled to use AVX2 instructions, but these are available on your machine and could speed up CPU computations.
    The TensorFlow library wasn't compiled to use FMA instructions, but these are available on your machine and could speed up CPU computations.
    ~~~~

