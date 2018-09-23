package com.gildedrose

class GildedRose(var items: Array[Item]) {


  def updateQuality() {
    items = items.map(item => {

      item match {
        case i if i.name.equals("Aged Brie") => {
          item.copy(
            sellIn = item.sellIn - 1,
            quality = (item.quality + 1).min(50)
          )
        }

        case i if i.name.equals("Backstage passes to a TAFKAL80ETC concert") => {
          item.copy(
            sellIn = item.sellIn - 1,
            quality = (item.sellIn match {
              case s if s > 10 => item.quality + 1
              case s if s <= 10 & s > 5 => item.quality + 2
              case s if s <= 5 & s > 0 => item.quality + 3
              case _ => 0
            }).min(50)
          )
        }

        case i if i.name.equals("Sulfuras, Hand of Ragnaros") => {
          item.copy()
        }

        case i if i.name.startsWith("Conjured") => {
          item.copy(
            sellIn = item.sellIn - 1,
            quality = (item.sellIn match {
              case s if s > 0 => item.quality - 2
              case _ => item.quality - 4
            }).max(0)
          )
        }

        case _ => {
          item.copy(
            sellIn = item.sellIn - 1,
            quality = (item.sellIn match {
              case s if s > 0 => item.quality - 1
              case _ => item.quality - 2
            }).max(0)
          )
        }
      }

    })
  }
}
