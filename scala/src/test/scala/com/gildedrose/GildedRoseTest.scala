package com.gildedrose

import org.scalatest._

class GildedRoseTest extends FlatSpec with Matchers {

  def mixedNonLegendaryItems = new {
      val items = Array[Item](
        new Item("+5 Dexterity Vest", 10, 20),
        new Item("Cheap +5 Dexterity Vest", 10, 1),
        new Item("Aged Brie", 2, 0),
        new Item("Aged Brie", 2, 49),
        new Item("Elixir of the Mongoose", 5, 7),
        new Item("Elixir of the Mongoose over due", 0, 7),
        new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        new Item("Conjured Mana Cake", 3, 6),
        new Item("Conjured Mana Cake", 0, 6)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "construct the items array with the proper order" in {
    val f = mixedNonLegendaryItems
    (f.app.items.map(_.name)) should equal (f.items.map(_.name))
  }

  it should "lower after each day the sellIn" in {
    val f = mixedNonLegendaryItems
    f.app.updateQuality()
    (f.app.items.map(_.sellIn)) should equal (f.items.map(_.sellIn - 1))
  }

  it should "never degrade the quality of items to negative" in {
    val f = mixedNonLegendaryItems
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    (f.app.items.filter(_.quality < 0).length) should equal (0)
  }

  it should "keep the quality of items always below 50" in {
    val f = mixedNonLegendaryItems
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    (f.app.items.filter(_.quality > 50).length) should equal (0)
  }

  def regularItems = new {
    val items = Array[Item](
      new Item("+5 Dexterity Vest", 10, 20),
      new Item("Cheap +5 Dexterity Vest", 10, 1),
      new Item("Elixir of the Mongoose", 5, 7),
      new Item("Elixir of the Mongoose over due", 0, 7)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "degrade the quality of regular items by one if sellIn > 0" in {
    val f = regularItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn >= 0).map(_.quality)) should equal (f.items.filter(_.sellIn > 0).map(_.quality - 1))
  }

  it should "degrade the quality of regular items by two if sellIn <= 0" in {
    val f = regularItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn < 0).map(_.quality)) should equal (f.items.filter(_.sellIn <= 0).map(_.quality - 2))
  }

  def brieItems = new {
    val items = Array[Item](
      new Item("Aged Brie", 2, 0),
      new Item("Aged Brie", 2, 49)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "upgrade the quality of 'Aged Brie' by one if quality < 50" in {
    val f = brieItems
    f.app.updateQuality()
    (f.app.items.filter(_.quality < 50).map(_.quality)) should equal (f.items.filter(_.quality < 49).map(_.quality + 1))
  }

  def sulfurasItems = new {
    val items = Array[Item](
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
      new Item("Sulfuras, Hand of Ragnaros", -1, 80)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "keep the sellIn of 'Sulfuras, Hand of Ragnaros' invariant" in {
    val f = sulfurasItems
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    (f.app.items.map(_.sellIn)) should equal (f.items.map(_.sellIn))
  }

  it should "keep the quality of 'Sulfuras, Hand of Ragnaros' invariant" in {
    val f = sulfurasItems
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    f.app.updateQuality()
    (f.app.items.map(_.quality)) should equal (f.items.map(_.quality))
  }

  def backstageItems = new {
    val items = Array[Item](
      new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 40),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 40),
      new Item("Backstage passes to a TAFKAL80ETC concert", 1, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 49)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "upgrade the quality of 'Backstage passes to a TAFKAL80ETC concert' by one if sellIn > 10" in {
    val f = backstageItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn >= 10).map(_.quality)) should equal (f.items.filter(_.sellIn > 10).map(_.quality + 1))
  }

  it should "upgrade the quality of 'Backstage passes to a TAFKAL80ETC concert' by two if 5 < sellIn <= 10" in {
    val f = backstageItems
    f.app.updateQuality()
    (f.app.items.filter(i => (i.sellIn < 10) & (i.sellIn >= 5)).map(_.quality)) should equal (
      f.items.filter(i => (i.sellIn <= 10) & (i.sellIn > 5)).map(i => (i.quality + 2).min(50)))
  }

  it should "upgrade the quality of 'Backstage passes to a TAFKAL80ETC concert' by three if 0 < sellIn <= 5" in {
    val f = backstageItems
    f.app.updateQuality()
    (f.app.items.filter(i => (i.sellIn < 5) & (i.sellIn >= 0)).map(_.quality)) should equal (
      f.items.filter(i => (i.sellIn <= 5) & (i.sellIn > 0)).map(i => (i.quality + 3).min(50)))
  }

  it should "degrade the quality of 'Backstage passes to a TAFKAL80ETC concert' to 0 if sellIn <= 0" in {
    val f = backstageItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn < 0).map(_.quality)) should equal (f.items.filter(_.sellIn <= 0).map(i => 0))
  }

  def conjuredItems = new {
    val items = Array[Item](
      new Item("Conjured Mana Cake", 3, 6),
      new Item("Conjured Mana Pie", 3, 6),
      new Item("Conjured Mana Sweetroll", 0, 6)
    )
    val test_items = items.map(_.copy())
    val app = new GildedRose(test_items)
  }

  it should "degrade the quality of conjured items by two if sellIn > 0" in {
    val f = conjuredItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn >= 0).map(_.quality)) should equal (f.items.filter(_.sellIn > 0).map(_.quality - 2))
  }

  it should "degrade the quality of conjured items by four if sellIn <= 0" in {
    val f = conjuredItems
    f.app.updateQuality()
    (f.app.items.filter(_.sellIn < 0).map(_.quality)) should equal (f.items.filter(_.sellIn <= 0).map(_.quality - 4))
  }
}