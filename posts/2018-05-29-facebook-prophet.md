---
title: Facebook Prophet 模型簡介
tags: machine-learning
---

<section>
去年 Facebook 公開了一個時間序列[^ts]的預測模型 - [Prophet](https://facebook.github.io/prophet/)，
這篇文章簡介它的構造及運作原理。

</section>
[^ts]: 時間序列是按照時間順序的資料點所構成，舉幾個常見的時間序列是股價、每小時氣溫、一個網站每分鐘的點擊數…等。

# The Model

## Additional Model

Prophet 用了一個很經典的加法模型來描述資料

$$ y(t) = g(t) + s(t) + h(t) + ε_t $$

在所有時間點 $t$ 的數值（$y(t)$） 是由：

  * 趨勢的影響 $g(t)$、
  * 季節性的影響 $s(t)$、
  * 假日的影響 $h(t)$ 及
  * 誤差 $ε_t$

這四個成份所加總而來。

## Trend

Prophet 提供了兩種不同的趨勢函數供使用者使用。

  * Linear 用於不會飽和的預測[^saturation]

    $$g(t) = k \cdot t + m$$

  * Logistic 用於會飽和的預測

    $$g(t) = \frac{C(t)}{1+\exp(-k \cdot (t-m))}$$

    其中 $C(t)$ 是不同時間點的「容量」上界。


[^saturation]: 比方說 Facebook 的使用者人數不會超過當時可以連網的總人口數，
 這個容量上界可能會隨著時間、市場變化而有所改變。

### Change Points

除了單純的趨勢函數之外， Prophet 還引入了轉折點(change point)的想法，
也就是讓趨勢函數在不同的時間區間內，能有不同的增長率 $k$。

^[{-} $\mathbf{1}(\text{expr})$ 是
$f(t) = \begin{cases} 1 & \text{if expr is true} \\ 0 & \text{otherwise} \end{cases}$
的一個簡寫方法，被稱為 indicator function.]

$$k(t) = k_0 + \sum_{j} \mathbf{1}(t \leq t_j) \cdot δ_j$$


其中 $t_j$ 表示 $j$ 這個轉折點的時間，
也就是一開始的基礎 $k_0$ 加上接下來的所有變化。

另外，調整了 $k$ 就得調整 $m$，
調整後的公式就是[原文][paper]中的 (3) 跟 (4)，這邊就不列出。


[paper]: https://peerj.com/preprints/3190/


## Seasonality

Prophet 應用 Fourier series 來描述季節性[^seasonality]

[^seasonality]: 季節性是固定週期所帶來的影響，比方說：
餐廳的來客數會隨的週一到週日有著類似的規律、通勤時間交通比較壅塞…等。

$$s(t) = \sum_{n}^{N} \left( a_n \cos \left(\frac{2\pi nt}{P}\right) +
b_n \sin\left(\frac{2\pi nt}{P} \right)\right)$$

其中 $P$ 是週期的長度（年的週期是 365.25 天，周的週期是 7 天…），
透過控制 $a_n, b_n$ 參數，可以近似出任何一個周期函數。

## Holiday

Prophet 假設每種假日 $D_i$ 都會帶來不同的影響。

$$ h(t) = \sum_{i}^{D} κ_i \cdot \mathbf{1}(t \in D_i) $$

# Fitting the model

前面定義好了模型的結構，但是要怎麼樣挑選參數呢？
Prophet 應用了機率模型來描述這些參數，

$$\begin{gathered}
k_0, m & \sim &  \mathcal{N}(0,5) \\
δ_i & \sim &  Laplace(0, τ) \\
κ_i, a_i, b_i & \sim &  \mathcal{N}(0, σ) \\
ε_t & \sim &  \mathcal{N}(0, 0.5)\\
y_t & \sim & \mathcal{N}\left( g(t)+s(t)+h(t), ε_t \right) \\
\end{gathered}$$

接著再用 L-BFGS 演算法找出這些參數的最大後驗估計(Maximum a posterori estimation; MAP) 或是用 MCMC 來 inference 出每個參數的後驗機率。

找出這些參數之後，我們就可以拿來預測未來的數值。

# Conclusion

Prophet 提供了一個麻瓜也會用^[不過也得先會操作 R 或是 Python]的預測模型：

  * 只需要想預測的資料就能動
  * 可以允許空資料點（傳統的統計方法不行）
  * 轉折點可以手動指定，也可以自動預測
  * 可以考慮會飽和的情況
  * $τ, σ$ 兩個 hyper-parameter 可以微調模型
  * 因為是加法模型，結果能很簡單的解釋

雖然沒辦法包含更深入的 feature（天氣狀況、特定事件），但是如上述優點，
這麼模型還是可以在商業時間序列預測上達到不錯的成效。
