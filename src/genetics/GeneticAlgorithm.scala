package genetics

import java.util.concurrent.{CountDownLatch, ExecutorService, Executors, TimeUnit}

import genetics.dxc.Calborn
import genetics.geometry._
import genetics.genes._

import scala.collection.mutable.ListBuffer

object GeneticAlgorithm {

  def geneticAlgorithm[T](fitnessFun: T => Double, geneToObj: List[Gene] => T, SimpleInput:List[Gene]):T={
    //设置部分 以下参数均经过过优化调整
    //可在180秒时间限制内产生尽可能最佳的答案
    val st_size: Int = 50 //种群大小
    val st_gene_num: Int = SimpleInput.length //染色体长度（基因数量）
    val st_maxGeneration: Int = 1000 //最大代数
    val st_tournament_playersize: Int = 3 //锦标赛选手数
    val st_Pca:Double = 0.8 //对个体交叉互换发起概率 P（crossover）
    val st_Pcb:Double = 0.9 //每个基因交叉互换概率 P（crossover）
    val st_crossoverRate:Double = 0.2 //交叉互换常数
    val st_Pva:Double = 0.1 //对个体变异发起概率 P（variation）
    val st_Pvb:Double = 0.8 //每个基因变异概率 P（variation）
    val st_variationRate:Double = 0.8 //变异常数

    //初始化变量
    var gl_population: Array[Animal] = new Array(st_size) //整个种群 Array
    var gl_generation: Int = 0

    //计时器开始
    val gl_starttime=System.nanoTime

    //初始化种群
    for(animal_no <- 0 until st_size){
      var lcl_genes: List[Gene] = List()
      for(gene_no <- 0 until st_gene_num){
        lcl_genes = lcl_genes :+ new Gene((new util.Random).nextGaussian())
      }
      gl_population(animal_no) = new Animal(lcl_genes)
    }
    var gl_bestfitness: Double = 0

    while(gl_generation < st_maxGeneration ){
      //&& !((gl_bestfitness < 1) && (gl_bestfitness > 0.99999999))
      //在有需要的时候，可以将上一行的式子加入while条件内减少运算时间
      //代数+1
      gl_generation = gl_generation + 1


      //val lcl_starttime1=System.nanoTime

      //评价个体
      for(animal <- gl_population){
        animal.fitness = fitnessFun(geneToObj(animal.genes))
      }

      //val lcl_endtime1=System.nanoTime
      //val lcl_starttime2=System.nanoTime

      //锦标赛算法 三选一 需要改成多线程
      var gl_nextgen_population: Array[Animal] = new Array(st_size)
      //对于N个个体，进行N/2 - 1 次竞标赛 直接保留最佳的2个个体

      for(i <- 0 until (st_size/2)-3){

        //尝试用多线程的方式节省运行时间，以在保证质量的同时保证运行时间不会超过180秒限制
        //受到时间的限制，并没有能力完成优化，此段代码反而会减慢运行时间
//              val threadPool:ExecutorService= Executors.newCachedThreadPool
//              val ctl: CountDownLatch = new CountDownLatch(st_size)
//              try{
//                for(animal <- gl_population){
//                  threadPool.execute(new Calborn(gl_population,st_tournament_playersize,gl_nextgen_population,st_Pca,st_Pcb, st_Pva,st_Pvb,st_crossoverRate,st_variationRate,i,ctl))
//                }
//                ctl.await(100, TimeUnit.MILLISECONDS)
//
//              } finally{
//                threadPool.shutdown()
//              }
        //多线程写法结束



        //选出父母的选手
        val lcl_father:Animal =find_best_fitness(sample(gl_population,st_tournament_playersize))
        val lcl_mother:Animal =find_best_fitness(sample(gl_population,st_tournament_playersize))


        //开始生殖 初始化孩子
        val child1genes:Array[Gene] = born_child(lcl_father,lcl_mother).toArray
        val child2genes:Array[Gene] = born_child(lcl_father,lcl_mother).toArray

        //交叉互换
        if((new util.Random).nextGaussian() < st_Pca){
          for(i <- child1genes.indices){
            if((new util.Random).nextGaussian() < st_Pcb){
              child1genes(i) = new Gene(st_crossoverRate * lcl_mother.genesArray(i).geneValue + (1-st_crossoverRate) * lcl_father.genesArray(i).geneValue)
              child2genes(i) = new Gene(st_crossoverRate * lcl_father.genesArray(i).geneValue + (1-st_crossoverRate) * lcl_mother.genesArray(i).geneValue)
            }
          }
        }

        //变异 对两个个体都进行变异操作
        if((new util.Random).nextGaussian() < st_Pva) {
          for (i <- child1genes.indices) {
            if ((new util.Random).nextGaussian() < st_Pvb) {
              val R: Double = (new util.Random).nextGaussian()
              if ((new util.Random).nextInt(2) % 2 == 0) {
                child1genes(i) = new Gene(lcl_father.genesArray(i).geneValue + st_variationRate * (1 - lcl_father.genesArray(i).geneValue) * R)
              } else {
                child1genes(i) = new Gene(lcl_father.genesArray(i).geneValue - st_variationRate * (lcl_father.genesArray(i).geneValue - 0) * R)
              }
            }
          }

          for (i <- child2genes.indices) {
            if ((new util.Random).nextGaussian() < st_Pvb) {
              val R: Double = (new util.Random).nextGaussian()
              if ((new util.Random).nextInt() % 2 == 0) {
                child2genes(i) = new Gene(lcl_mother.genesArray(i).geneValue + st_variationRate * (1 - lcl_mother.genesArray(i).geneValue) * R)
              } else {
                child2genes(i) = new Gene(lcl_mother.genesArray(i).geneValue - st_variationRate * (lcl_mother.genesArray(i).geneValue - 0) * R)
              }
            }
          }
        }


        //生殖部分结束
        gl_nextgen_population(2*i)=new Animal(child1genes.toList)
        gl_nextgen_population((2*i)+1)=new Animal(child2genes.toList)
      }
      //val lcl_endtime2=System.nanoTime

      //精英保留制度

      val lcl_sorted_population: Array[Animal] = gl_population.sortWith(comparefitness)

      gl_nextgen_population(st_size-6) = new Animal(lcl_sorted_population(0).genes)
      gl_nextgen_population(st_size-5) = new Animal(lcl_sorted_population(1).genes)
      gl_nextgen_population(st_size-4) = new Animal(lcl_sorted_population(2).genes)
      gl_nextgen_population(st_size-3) = new Animal(born_child(lcl_sorted_population(0),lcl_sorted_population(2)))
      gl_nextgen_population(st_size-2) = new Animal(born_child(lcl_sorted_population(0),lcl_sorted_population(1)))
      gl_nextgen_population(st_size-1) = new Animal(born_child(lcl_sorted_population(0),lcl_sorted_population(1)))

      //本地调试用Print，中+英文版
      //println("Fitness runtime："+((lcl_endtime1-lcl_starttime1)/1000000d).toString+"ms  Multiplication runtime："+((lcl_endtime2-lcl_starttime2)/1000000d).toString+"ms")
      //println("适应性计算耗时："+((lcl_endtime1-lcl_starttime1)/1000000d).toString+"ms  生殖耗时："+((lcl_endtime2-lcl_starttime2)/1000000d).toString+"ms")
      //println("第"+gl_generation.toString+"代繁衍完毕，本代最佳:"+geneToObj(lcl_sorted_population(0).genes).toString+" 适应值："+lcl_sorted_population(0).fitness.toString)
      //println("The "+gl_generation.toString+" generation done，Best in this generation: "+geneToObj(lcl_sorted_population(0).genes).toString+" Fitness："+lcl_sorted_population(0).fitness.toString)
      gl_bestfitness = lcl_sorted_population(0).fitness
      gl_population = gl_nextgen_population
    }

    //结束计时
    val gl_endtime=System.nanoTime
    val delta = gl_endtime-gl_starttime
    //println("")
    //println("运行结束，共计运行"+gl_generation.toString+"代，总耗时："+(delta/1000000d).toString+"ms代均耗时："+(delta/1000000d/gl_generation).toString+"ms")

    // 返回最佳个体
    for(animal <- gl_population){
      animal.fitness = fitnessFun(geneToObj(animal.genes))
    }
    //println("本代最佳:"+geneToObj(find_best_fitness(gl_population.toList).genes).toString+" 适应值："+find_best_fitness(gl_population.toList).fitness.toString)
    geneToObj(find_best_fitness(gl_population.toList).genes)
  }

  //生孩子~
  def born_child(F:Animal,M:Animal):List[Gene]={
    var result:List[Gene]= List()
    for(i<- F.genes.indices){
      result = result :+ new Gene(random_between_Double(F.genes(i).geneValue,M.genes(i).geneValue))
    }
    result
  }

  //按范围取随机数·改
  def random_between_Double(a:Double,b:Double):Double={
    Math.min(a,b)*0.99 + (new util.Random).nextGaussian() * Math.abs(a-b)*1.01
  }
  //比较两个个体的适应度 大的为True
  def comparefitness(a1: Animal, a2: Animal): Boolean = {
    a1.fitness > a2.fitness
  }

  //找出列表里适应性最佳的个体
  def find_best_fitness(orig:List[Animal]):Animal={
    var best:Animal = orig.head
    for(i <- orig){
      if(i.fitness>best.fitness){
        best = i
      }
    }
    best
  }
  //取样函数 从全部里取N个
  def sample(orig:Array[Animal],sampleNumber:Int):List[Animal]={
    var resultList:List[Int]=List()
    var resultList_Animal:List[Animal]=List()
    while(resultList.length<sampleNumber){
      val randomNum=(new util.Random).nextInt(orig.length)
      if(!resultList.contains(randomNum)){
        resultList=resultList:::List(randomNum)
        resultList_Animal = resultList_Animal :+ orig(randomNum)
      }
    }
    //println(resultList(0).toString+","+resultList(1).toString+","+resultList(2).toString)
    resultList_Animal
  }
  def linearRegression(points:List[Point]):Line={

    val par1 = linearRegression_fitness_FunGen(points)
    val par2 = linearRegression_geneToLine_FunGen()
    val par3 = List(new Gene(0),new Gene(0))
    geneticAlgorithm(par1,par2,par3)
  }

  def linearRegression_geneToLine_FunGen():List[Gene] => Line={
    Genes:List[Gene] => new Line(Genes.head.decode,Genes.apply(1).decode)
  }

  def main(args: Array[String]): Unit = {

  }


  def linearRegression_fitness_FunGen(points:List[Point]): Line => Double = {
    input_line:Line => {
      var dist:Double = 0.0
      for(point <- points){
        dist = dist + dist_line_point(input_line.slope,-1,input_line.yIntercept,point.x,point.y)
      }
      fitness_encode(dist)
    }
  }

  //两点间距离公式(2019-03-20 20:27:49更新： 按题目要求，改成点到直线Y的差值）
  def dist_line_point(A:Double,B:Double,C:Double,x0:Double,y0:Double): Double ={
    //Math.abs((A*x0)+(B*y0)+C)/Math.sqrt(A*A+B*B)
    //2019-03-20 20:28:22

    Math.abs((A*x0+C)-y0)
  }

  //基因数编码 因题目只会考虑正负100内情况，所以角度值选择正负89.5度
  def gene_encode(number:Double):Double={
    //Math.max(Math.min((Math.atan(number)/1.56206969/2)+0.5,1),0)
    Math.max(Math.min((number/100)-0.5,1),0)
  }

  //适应度编码 为保证完整映射，角度值选择了90.01度
  def fitness_encode(number:Double):Double={
    1-Math.max(Math.min(Math.atan(Math.abs(number))/1.57079634,1),0)
    //Math.max(Math.min(1-(Math.abs(number)/100),1),0)
  }

  def polynomialRegression(points:List[Point],level:Int):Polynomial={

    val par1 = polynomialRegression_fitness_FunGen(points)
    val par2 = polynomialRegression_geneToLine_FunGen()
    var par3:List[Gene]= List(new Gene(0.0))
    for(i <- 0 until level){
      par3 = par3 :+ new Gene(0.0)
    }
    geneticAlgorithm(par1,par2,par3)
  }

  def polynomialRegression_fitness_FunGen(points:List[Point]): Polynomial => Double = {
    input_Polynomial: Polynomial => {
      var dist: Double = 0.0
      for (point <- points) {
        dist = dist + Math.abs(input_Polynomial.evaluate(point.x)-point.y)
      }
      fitness_encode(dist)
    }
  }

  def polynomialRegression_geneToLine_FunGen():List[Gene] => Polynomial={
    Genes:List[Gene] => {
      var newlist:List[Double] = List()
      for(i <- Genes){
        newlist = newlist :+ i.decode
      }
      new Polynomial(newlist)
    }
  }


}
