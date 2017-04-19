import java.io.File
import java.util

import com.github.tototoshi.csv._

import scala.collection.immutable.ListMap
import scala.collection.JavaConverters._
import javax.sound.sampled._

/**
  * Created by ewert on 4/18/2017.
  */
object FunWithTiles
{
	def main(args: Array[String]): Unit =
	{
		val beginTime = System.currentTimeMillis()
		val reader = CSVReader.open(new File("resources\\tile_placements.csv"))
		val colorMap = scala.collection.mutable.Map[Int, Int]()
		val tileList = new util.ArrayList[Tile]()
		val noteList = new util.ArrayList[Note]()

		val it = reader.iterator
		var count = 0

		while (it.hasNext)
		{
			count += 1
			val test = it.next()
			//colorMap(test(4).toInt) += 1
			//tsList.add(test(0).toLong/1000)
			tileList.add(new Tile(test(0).toLong/1000, test(4).toInt))
			if ((count % 50000) == 0)
			{
				println(count + " rows read in " + (System.currentTimeMillis() - beginTime) / 1000 + " seconds")
			}
		}

		val newList = tileList.asScala.toList.sortWith(_.getTimeStamp() < _.getTimeStamp())
		//newList.foreach(rec => println(rec.getTimeStamp()))

		var i = 0

		while (i < 16)
		{
			colorMap += (i -> 0)
			i += 1
		}

		var usedTs = 0L
		var usedKey = 0
		var noteCnt = 0

		val it2 = newList.iterator
		while (it2.hasNext)
		{
			val temp = it2.next

			colorMap(temp.colorNum) += 1
			if ((temp.getTimeStamp() % 500 == 0) && temp.getTimeStamp() != usedTs)
			{
				val test = ListMap(colorMap.toSeq.sortBy(_._2):_*)

				if (noteCnt < 4)
				{
					val (k,v) = test.head
					noteList.add(new Note(temp.getTimeStamp(), k, v))
					usedKey = k
					noteCnt += 1
				}
				else
				{
					val it = test.iterator
					it.next()
					val (k,v) = it.next()
					noteList.add(new Note(temp.getTimeStamp(), k, v))
					usedKey = k
					noteCnt = 0
				}
				usedTs = temp.getTimeStamp()

				colorMap.clear
				i = 0
				while (i < 16)
				{
					colorMap += (i -> 0)
					i += 1
				}
			}
		}

		playSong(noteList)
	}

	def playSong(noteList : util.ArrayList[Note]): Unit =
	{
		val it = noteList.iterator()
		var inSound = new File("")

		while (it.hasNext)
		{
			val temp = it.next()

			temp.noteKey match
			{
				case 0 => inSound = new File("resources\\piano_sounds\\a.wav")
				case 1 => inSound = new File("resources\\piano_sounds\\b.wav")
				case 2 => inSound = new File("resources\\piano_sounds\\bb.wav")
				case 3 => inSound = new File("resources\\piano_sounds\\c.wav")
				case 4 => inSound = new File("resources\\piano_sounds\\cc.wav")
				case 5 => inSound = new File("resources\\piano_sounds\\d.wav")
				case 6 => inSound = new File("resources\\piano_sounds\\e.wav")
				case 7 => inSound = new File("resources\\piano_sounds\\eb.wav")
				case 8 => inSound = new File("resources\\piano_sounds\\f.wav")
				case 9 => inSound = new File("resources\\piano_sounds\\ff.wav")
				case 10 => inSound = new File("resources\\piano_sounds\\g.wav")
				case 11 => inSound = new File("resources\\piano_sounds\\e.wav")
				case 12 => inSound = new File("resources\\piano_sounds\\e.wav")
				case 13 => inSound = new File("resources\\piano_sounds\\e.wav")
				case 14 => inSound = new File("resources\\piano_sounds\\gg.wav")
				case 15 => inSound = new File("resources\\piano_sounds\\e.wav")
			}

			val audioIn = AudioSystem.getAudioInputStream(inSound)
			val clip = AudioSystem.getClip
			clip.open(audioIn)
			clip.start
			println(temp.timeStamp + " : " + temp.noteKey + " : " + temp.noteValue)
			Thread sleep(200)
		}
	}

	private class Tile(ts : Long, num : Int)
	{
		var timeStamp = ts
		var colorNum = num

		def getTimeStamp(): Long = { timeStamp }

	}

	private class Note(ts : Long, key : Int, value : Int)
	{
		val timeStamp = ts
		val noteKey = key
		val noteValue = value
	}
}
