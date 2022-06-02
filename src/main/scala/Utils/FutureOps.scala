package Utils

import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success}

object FutureOps {
  import scala.concurrent.ExecutionContext.Implicits.global

  /**
    * Create a future that will complete after `mean` seconds (with some variation based on `std`). The future has
    * `successPercentage` chance of succeeding.
    * @param mean the mean of the gaussian curve
    * @param std the std of the gaussian curve
    * @param successRate the chance that the future succeed (0.0 the future always fails, 1.0 the future always succeed)
    * @return the future
    */
  def randomSchedule(mean: Duration, std: Duration = 0.second, successRate: Double = 1.0): Future[Unit] =
    val succeed = Random.nextDouble() < successRate
    val waitDuration = std * Random.nextGaussian() + mean
    Future{
      Thread.sleep(waitDuration.toMillis)
      if !succeed then throw new Exception("Failed future")
      ()
    }
}
