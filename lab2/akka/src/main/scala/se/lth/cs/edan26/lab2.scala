import akka.actor._
import akka.pattern.{ask, gracefulStop}
import akka.util.Timeout
import java.util.BitSet

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class Random(seed: Int) {
	var w = seed + 1
	var z = seed * seed + seed + 2

	def nextInt() =
	{
		z = 36969 * (z & 65535) + (z >> 16)
		w = 18000 * (w & 65535) + (w >> 16)

		(z << 16) + w
	}
}

class Controller(val cfg: Array[ActorRef], print: Int, system: ActorSystem) extends Actor {
	var started = 0
	var begin: Long   = 0
	var changeCounter = 0

	// LAB 2: The controller must figure out when
	//        to terminate all actors somehow.

	def receive: PartialFunction[Any, Unit] = {
		case Start() => {
			begin = System.nanoTime()
		}

		case Ready() => {
			started += 1
			//println("controller has seen " + started)
			if (started == cfg.length) {
				changeCounter = cfg.length
				for (u <- cfg)
					u ! new Go
			}
		}

		case ChangeAdded() => {
			changeCounter += 1
		}

		case ChangeProcessed() => {
			changeCounter -= 1
			if (changeCounter == 0) { // No more changes are being made
				if (print != 0) {
					implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
					implicit val timeout: Timeout = Timeout(1 seconds)
					for (v <- cfg) {
						Await.result(v.ask(Print()), 1 seconds)
						gracefulStop(v, 0.5 seconds)
					}
				} else {
					cfg.foreach(v => v ! PoisonPill)
				}
				val end = System.nanoTime()
				println("T = " + (end-begin)/1e9 + " s")
				system.terminate()
			}
		}
	}
}

class Vertex(val index: Int, s: Int, val controller: ActorRef) extends Actor {
	var pred: List[ActorRef] = List()
	var succ: List[ActorRef] = List()
	val uses               = new BitSet(s)
	val defs               = new BitSet(s)
	var in                 = new BitSet(s)
	var out                = new BitSet(s)

	def receive: PartialFunction[Any, Unit] = {

		case IsDef(i: Int, s: Int) => {
			if (!defs.get(s)) {
				sender ! IsDef(i, s)
			} else {
				sender ! false
			}
		}

		case IsUses(i: Int, s: Int) => {
			if (!uses.get(s)) {
				sender ! IsUses(i, s)
			} else {
				sender ! false
			}
		}

		case SetDef(s: Int) => {
			defs.set(s)
		}
		case SetUses(s: Int) => {
			uses.set(s)
		}
		case ConnectPred(that: ActorRef) => {
			this.pred = that :: this.pred
		}

		case Connect(that: ActorRef) => {
			this.succ = that :: this.succ
		}

		case Start() => {
			controller ! Ready()
			//println("started " + index)
		}

		case Go() => {
			// LAB 2: Start working with this vertex.
			self ! VertexChange(new BitSet())
		}

		case VertexChange(in: BitSet) => {
			out.or(in)
			self ! PropagateChange()
		}

		case PropagateChange() => {
			val oldIn = in
			in = new BitSet()
			in.or(out)
			in.andNot(defs)
			in.or(uses)
			if (!in.equals(oldIn)){
				for (v <- pred) {
					controller ! ChangeAdded()
					v ! VertexChange(in)
				}
			}
			controller ! ChangeProcessed()
		}

		case Print() => {
			print
			sender ! true
		}
	}

	def printSet(name: String, index: Int, set: BitSet) {
		System.out.print(name + "[" + index + "] = { ")
		for (i <- 0 until set.size)
			if (set.get(i))
				System.out.print("" + i + " ")
		println("}")
	}

	def print = {
		printSet("use", index, uses)
		printSet("def", index, defs)
		printSet("in", index, in)
		println("")
	}
}

object Driver {
	val rand    = new Random(1)
	var nactive = 0
	var nsym    = 0
	var nvertex = 0
	var maxsucc = 0

	def makeCFG(cfg: Array[ActorRef]) {

		cfg(0) ! new Connect(cfg(1))
		cfg(1) ! new ConnectPred(cfg(0))

		cfg(0) ! new Connect(cfg(2))
		cfg(2) ! new ConnectPred(cfg(0))

		for (i <- 2 until cfg.length) {
			val p = cfg(i)
			val s = (rand.nextInt() % maxsucc) + 1

			for (j <- 0 until s) {
				val k = cfg((rand.nextInt() % cfg.length).abs)
				p ! new Connect(k)
				k ! new ConnectPred(p)
			}
		}
	}

	def makeUseDef(cfg: Array[ActorRef]) {
		for (i <- 0 until cfg.length) {
			for (j <- 0 until nactive) {
				val s = (rand.nextInt() % nsym).abs
				implicit val timeout: Timeout = Timeout(5 seconds)
				implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
				if (j % 4 != 0) {
					cfg(i)
						.ask(IsDef(i, s))
  					.onComplete {
							case Success(isDef: IsDef) => cfg(isDef.i) ! SetUses(isDef.s)
							case Success(fail: Boolean) => // Do noting
							case Failure(ex) => throw ex // We timed out, something is off
						}
				} else {
					cfg(i)
						.ask(IsUses(i, s))
						.onComplete {
							case Success(isUses: IsUses) => cfg(isUses.i) ! SetDef(isUses.s)
							case Success(fail: Boolean) => // Do noting
							case Failure(ex) => throw ex// We timed out, something is off
						}
				}
			}
		}
	}

	def main(args: Array[String]) {
		nsym           = args(0).toInt
		nvertex        = args(1).toInt
		maxsucc        = args(2).toInt
		nactive        = args(3).toInt
		val print      = args(4).toInt

		val system     = ActorSystem("Lab2")
		val cfg        = new Array[ActorRef](nvertex)
		val nsucc      = new Array[Int](nvertex)
		val controller = system.actorOf(Props(new Controller(cfg, print, system)), "controller")

		println("generating CFG...")
		for (i <- 0 until nvertex) {
			nsucc(i) = (rand.nextInt() % maxsucc).abs
			cfg(i) = system.actorOf(Props(new Vertex(i, nsym, controller)), "vertex-" + i)
		}

		makeCFG(cfg)
		println("generating usedefs...")
		makeUseDef(cfg)

		println("starting " + nvertex + " actors...")

		implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
		controller ! Start()
		cfg.foreach(v => v ! Start())
		system
			.whenTerminated
			.onComplete {
				case Success(terminated) => System.exit(0)
				case Failure(exception) =>
					exception.printStackTrace()
					System.exit(1)
			}
	}
}

// Used for controlling program flow
case class Print()
case class Start()
case class Ready()
case class Go()

// Used for building cfg
case class Connect(that: ActorRef)
case class ConnectPred(that: ActorRef)
case class IsDef(i: Int, s: Int)
case class IsUses(i: Int, s: Int)
case class SetDef(s: Int)
case class SetUses(s: Int)

// Used for calculating vertices
case class VertexChange(set: BitSet)
case class PropagateChange()
case class ChangeAdded()
case class ChangeProcessed()