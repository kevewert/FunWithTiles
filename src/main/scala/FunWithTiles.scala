import java.io.{ByteArrayInputStream, File, SequenceInputStream}
import java.nio.file.{Files, Paths}
import java.util

import com.github.tototoshi.csv._

import scala.collection.immutable.ListMap
import scala.collection.JavaConverters._
import javax.sound.sampled._

import scala.util.Random

object FunWithTiles
{
	def main(args: Array[String]): Unit =
	{
		val beginTime = System.currentTimeMillis()
		val reader = CSVReader.open(new File("resources\\tile_placements.csv"))
		val colorMap = scala.collection.mutable.Map[Int, Int]()
		val tileList = new util.ArrayList[Tile]()
		val noteList1 = new util.ArrayList[Note]()
		val noteList2 = new util.ArrayList[Note]()
		val noteList3 = new util.ArrayList[Note]()

		val it = reader.iterator
		var count = 0

		while (it.hasNext)
		{
			count += 1
			val test = it.next()
			//colorMap(test(4).toInt) += 1
			//tsList.add(test(0).toLong/1000)
			tileList.add(new Tile(test(0).toLong/1000, test(4).toInt))
			if ((count % 1000000) == 0)
			{
				println(count + " rows read in " + (System.currentTimeMillis() - beginTime) / 1000 + " seconds")
			}
		}

		val newList = tileList.asScala.toList.sortWith(_.getTimeStamp() < _.getTimeStamp())

		var i = 0

		while (i < 16)
		{
			colorMap += (i -> 0)
			i += 1
		}

		var usedTs = 0L
		var usedKey = 0
		var noteCnt = 0
		var noteCnt2 = 0

		val it2 = newList.iterator
		while (it2.hasNext)
		{
			val temp = it2.next

			colorMap(temp.colorNum) += 1
			if ((temp.getTimeStamp() % 500 == 0) && temp.getTimeStamp() != usedTs)
			{
				val test = ListMap(colorMap.toSeq.sortWith(_._2 > _._2):_*)

				if (noteCnt < 2)
				{
					val it = test.iterator

					val (k,v) = it.next()
					noteList1.add(new Note(temp.getTimeStamp(), k, v))

					val (q,r) = it.next()
					noteList2.add(new Note(temp.getTimeStamp(), q, r))

					if (noteCnt2 > 3)
					{
						val (o,p) = it.next()
						noteList3.add(new Note(temp.getTimeStamp(), o, p))
						noteCnt2 = 0
					}

					usedKey = k
					noteCnt += 1
					noteCnt2 += 1
				}
				else
				{
					val it = test.iterator
					it.next()

					val (k,v) = it.next()
					noteList1.add(new Note(temp.getTimeStamp(), k, v))

					val (q,r) = it.next()
					noteList2.add(new Note(temp.getTimeStamp(), q, r))

					if (noteCnt2 > 3)
					{
						val (o,p) = it.next()
						noteList3.add(new Note(temp.getTimeStamp(), o, p))
						noteCnt2 = 0
					}

					usedKey = k
					noteCnt = 0
					noteCnt2 += 1
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

		playSong(noteList1, noteList2, noteList3)
	}

	def playSong(noteList1 : util.ArrayList[Note], noteList2 : util.ArrayList[Note], noteList3 : util.ArrayList[Note]): Unit =
	{
		val it1 = noteList1.iterator()
		val it2 = noteList2.iterator()
		val it3 = noteList3.iterator()
		var inSound = Array[Byte]()
		var inSound2 = Array[Byte]()
		var inSound3 = Array[Byte]()
		var temp3 = new Note(0L, 0, 0)
		var count = 0

		println(noteList1.size() + " total notes to play.")

		while (it1.hasNext)
		{
			val temp1 = it1.next()
			val temp2 = it2.next()
			if (count % 4 == 0 && it3.hasNext)
				temp3 = it3.next()

			temp1.noteKey match
			{
				case 0 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 1 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 2 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 3 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 4 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 5 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 6 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 7 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 8 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 9 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 10 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 11 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 12 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 13 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 14 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 15 => inSound = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
			}

			temp2.noteKey match
			{
				case 0 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 1 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 2 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 3 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 4 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 5 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 6 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 7 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 8 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 9 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 10 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 11 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 12 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 13 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 14 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 15 => inSound2 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
			}

			temp3.noteKey match
			{
				case 0 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 1 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 2 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 3 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 4 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 5 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 6 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 7 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 8 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 9 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 10 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
				case 11 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\c.wav"))
				case 12 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\d.wav"))
				case 13 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\e.wav"))
				case 14 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\g.wav"))
				case 15 => inSound3 = Files.readAllBytes(Paths.get("resources\\piano_sounds\\a.wav"))
			}

			var length = 0
			if (inSound.length < inSound2.length && inSound.length < inSound3.length)
				length = inSound.length - 1
			else if (inSound2.length < inSound.length && inSound2.length < inSound3.length)
				length = inSound2.length - 1
			else
				length = inSound3.length - 1

			val result = new util.ArrayList[Byte]()

			if (count % 4 == 0)
			{
				for (i <- 0 to length)
				{
					val temp = ((inSound(i) + inSound2(i)) >> 1).toByte
					result.add(((temp + inSound3(i)) >> 1).toByte)
				}
			}
			else
			{
				for (i <- 0 to length)
				{
					val temp = ((inSound(i) + inSound2(i)) >> 1).toByte
					result.add(temp)
				}
			}

			val audioIn = AudioSystem.getAudioInputStream(new ByteArrayInputStream(result.asScala.toArray))
			val clip = AudioSystem.getClip

			clip.open(audioIn)

			clip.start
			if (count % 4 == 0)
				println(temp1.timeStamp + " : " + temp1.noteKey + " : " + temp1.noteValue + ", " +
					temp2.timeStamp + " : " + temp2.noteKey + " : " + temp2.noteValue + ", " +
					temp3.timeStamp + " : " + temp3.noteKey + " : " + temp3.noteValue)
			else
				println(temp1.timeStamp + " : " + temp1.noteKey + " : " + temp1.noteValue + ", " +
					temp2.timeStamp + " : " + temp2.noteKey + " : " + temp2.noteValue)

			count += 1
			Thread sleep(333)
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
