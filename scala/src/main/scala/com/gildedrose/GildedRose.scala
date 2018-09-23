package com.gildedrose

class GildedRose(var items: Array[Item]) {


  def updateQuality() {
    items.map(item => {

      item match {
        case i if i.name.equals("Aged Brie") => {
          i.sellIn = i.sellIn - 1
          i.quality = (i.quality + 1).min(50)
        }

        case i if i.name.equals("Backstage passes to a TAFKAL80ETC concert") => {
          i.sellIn = i.sellIn - 1
          i.quality = (i.sellIn match {
            case s if s >= 10 => i.quality + 1
            case s if s < 10 & s >= 5 => i.quality + 2
            case s if s < 5 & s >= 0 => i.quality + 3
            case _ => 0
          }).min(50)
        }

        case i if i.name.equals("Sulfuras, Hand of Ragnaros") => {}

        case i if i.name.startsWith("Conjured") => {
          i.sellIn = i.sellIn - 1
          i.quality = (i.sellIn match {
            case s if s >= 0 => i.quality - 2
            case _ => i.quality - 4
          }).max(0)
        }

        case i => {
          i.sellIn = i.sellIn - 1
          i.quality = (i.sellIn match {
            case s if s >= 0 => i.quality - 1
            case _ => i.quality - 2
          }).max(0)
        }
      }

    })
  }
}
