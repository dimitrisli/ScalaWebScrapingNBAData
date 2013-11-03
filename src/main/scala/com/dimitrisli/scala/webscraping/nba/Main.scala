package com.dimitrisli.scala.webscraping.nba

import org.jsoup.Jsoup
import scala.collection.JavaConversions._
import scala.collection.immutable.TreeMap

object Main {

  def main(args:Array[String]) {

    case class Player(name:String, surname:String, team:String, var rank:Int) extends Ordered[Player]{def compare(that:Player)= this.rank - that.rank}

    case class Stats(gp:String, mpg:String, pts:String, fgmfga:String, fgpc:String, pm3pa3:String, p3pc:String, ftmfta:String, ftpc:String)

    def webScrapNBALeagueLeaders(seasonUrl:String) = TreeMap( //apply order
        //get the table we're interested in
        Jsoup.connect(seasonUrl).get.select("div.span-6 table.tablehead").get(0)
          //exclude the repetitive header per 10 rows
          .select("tr:not(.colhead)").toList.map{
            //we don't create the Players directly because in the html
            //the ranking is missing for some of them. Therefore we first
            //return in a tuple all the Player ingredients (sic) waiting for the
            //zip in the next step to get the ranking artificially
            e => (e.select("td").get(1).select("a").text.split(" ")(0), //name
                  e.select("td").get(1).select("a").text.split(" ")(1), //surname
                  e.select("td").get(2).text, //team
              //we have all the data for Stats tho
              //so let's create the object
              new Stats(e.select("td").get(3).text, //gp
                e.select("td").get(4).text, //mgp
                e.select("td").get(5).text, //pts
                e.select("td").get(6).text, //fgmfga
                e.select("td").get(7).text, //fgpc
                e.select("td").get(8).text, //pm3pa3
                e.select("td").get(9).text, //p3pc
                e.select("td").get(10).text, //ftmfta
                e.select("td").get(11).text)) //ftpc
        }.zipWithIndex.map{
          //now we got the rank, time to create the players
          case((name, surname, team, stats), rank) =>(new Player(name, surname, team, rank+1),stats)
        }.toMap //this map doesn't maintain order
      .toSeq:_*) //TreeMap does

    def stopWatchWrapper(url:String)(f: String => Unit){
      val stopwatchStart = System.currentTimeMillis
      f(url)
      val stopwatchEnd = System.currentTimeMillis
      println(s"Completed in: ${(stopwatchEnd-stopwatchStart)/1000.0} secs")
    }

    val url = "http://espn.go.com/nba/statistics/player/_/stat/scoring-per-game/sort/avgPoints/year/2013/"
    stopWatchWrapper(url){ webScrapNBALeagueLeaders(_).foreach(println) }

  }
}
