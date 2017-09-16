---
title: 用 Python 計算經緯度對應的鄉鎮市區
tags: python, gis
image: https://i.imgur.com/MDrOi7ih.png
---

### TD;LR

這篇文章筆記最近朋友問的一個問題： 如何不靠外部 API 取得一個經緯度座標落在哪一個鄉鎮市區。

這邊應用政府 open data 的 [鄉鎮市區界線][opendata] 資料，加上 python 的 shapely 做範圍判斷、用 rtree 建立索引加速查詢。

[opendata]: https://data.gov.tw/dataset/7441


# 實作筆記

想法很單純，就是透過判斷座標(輸入的經緯度)有沒有落在多邊形(鄉鎮邊界)的範圍內。

## 資料來源

政府有提供[鄉鎮市區界線][opendata]資料，是一個 [Shapefile][shp-wiki]。
Python 要讀取 .shp 檔不難，有很多 library 可以做到，可以參考[這篇][py-shp]。

~~~~python
import fiona
from shapely.geometry import shape, Point

collection = fiona.open('./TOWN_MOI_1060525.shp')
~~~~

## 查詢

這邊我使用的是 `shapely` 這個套件來計算座標有無座落於某個多邊形。 所以先將 .shp 內的 geometry 建立成 shapely 的 Polygon 物件：

~~~~python
shapes = {}
properties = {}

for f in collection:
    town_id = int(f['properties']['TOWNCODE'])
    shapes[town_id] = shape(f['geometry'])
    properties[town_id] = f['properties']
~~~~

這樣就可以用 `polygon.contains(Point(x, y))`{.python} 判斷一個點有沒有座落於其中。對這所有的多邊形走訪一遍就可以知道座落於哪裡：

~~~~python
def search(x, y):
    return next((town_id
                 for town_id in shapes
                 if shapes[town_id].contains(Point(x, y))), None)
~~~~

## 加速查詢

上面的查詢很單純地測試全台灣 368 個鄉鎮市的多邊形了一遍，如果是小量的輸入資料應該還算是可以接受，
不過如果需要更細的比對（計算到村里）、或是有大量資料要計算的話，這樣的方法還是不夠有效率。

因為在查詢一個台北的座標時，其實不用考慮離他太遠的台中、台南…等其他鄉鎮市區。
基於這樣的想法，這邊用 R-Tree 這個[資料結構][rtree-ds] 來幫這些 bounding box 做索引：

~~~~python
from rtree import index
idx = index.Index()
for town_id, shape in shapes.items():
    idx.insert(town_id, shape.bounds)
~~~~

這樣就可以透過 `idx.intersection((x, y))`{.python} 來找出跟座標有交集的 bounding box。
我們只需要檢查座標落在其中的哪一個的多邊形即可：

~~~~python
def search(x, y):
    return next((town_id
                 for town_id in idx.intersection((x, y))
                 if shapes[town_id].contains(Point(x, y))), None)
~~~~

這樣查詢的時間就可以從 $O(N)$ 加快到 $O(log_M N + M)$ 了。
以鄉鎮市區來說，在我的筆電上，從本來每次平均需要 3.95ms 提升到每次平均 223 µs，至少快了 16 倍。


![以🇹🇼總統府來舉例，它落在萬華區跟中正區兩個 bounding boxes 裡面](https://i.imgur.com/MDrOi7i.png)


[shp-wiki]: https://en.wikipedia.org/wiki/Shapefile
[py-shp]: https://gis.stackexchange.com/questions/113799/how-to-read-a-shapefile-in-python
[rtree-ds]: https://en.wikipedia.org/wiki/R-tree

