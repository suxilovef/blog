---
title: Java-Juc
date: 2025-12-14 15:23:48
description: Java-Juc 并发
tags:
    - Java
    - Juc
categories:
    - Java
top_img: false
cover: /image/post_cover/java-juc-concurrency-min.svg
---

## A-001：并发

**有效利用每一份资源，精准控制每一次访问**

### B-001：Synchronized

1. 本质：引入上级：即当前级无法有效区分，引入上级控制
2. 用法：锁对象、锁类
3. 变量安全分析：成员变量、静态变量、局部变量
   1. 使用合适的方法修饰符，避免子类重写父类，使得局部变量可能出现并发问题（闭原则）
4. 线程安全类：String、Integer、StringBuffer、Random、Vector、Hashtable、java.util.concurrent
   1. 多个线程调用它们同一个实例的某个方法时，是线程安全的：每个方法是原子的，多个方法的组合不是原子的
   2. String、Integer都是不可变类，因为其内部的状态不可以改变，因此它们的方法都是线程安全的。原理：每次改变都是创建新的对象返回
   3. 无状态设计

#### Monitor

1. 本质：引入上级：即当前级无法有效区分，引入上级控制
2. Monitor：操作系统层面：本质上是一个资源管理者

#### 轻量级锁相关：

JVM层面

1. 锁流程
   1. 创建锁记录LR（Lock Record）对象，（displaced mark word：备份对象当前的 Mark Word，owner：指向加锁的对象）
   2. 让锁记录中的Object reference指向锁对象，并尝试用cas替换Object的Mark Word为LR指针，将Mark Word的值存入锁记录
   3. 如果cas（原子操作）替换成功，对象头中存储了锁记录地址和状态00，表示由该线程给对象加锁
   4. cas失败：
      1. 如果是其它线程已经持有了该Object的轻量级锁，这时表明有竞争，进入锁膨胀过程
      2. 如果是自己执行了synchronized锁重入，那么再添加一条Lock Record作为重入的计数
   5. 当退出synchronized代码块（解锁时）如果有取值为null的锁记录，表示有重入，这时重置锁记录，表示重入计数减一
   6. 当退出synchronized代码块（解锁时）锁记录的值不为null，这时使用cas将Mark Word的值恢复给对象头
      1. 成功，则解锁成功
      2. 失败，说明轻量级锁进行了锁膨胀或已经升级为重量级锁，进入重量级锁解锁流程。
2. 锁膨胀：thread-1进行轻量级加锁时，thread-0已经对该对象加了轻量级锁，升级为重量级锁
   1. 即为Object对象申请Monitor锁，让Object指向重量级锁地址
   2. 然后自己进入Monitor的EntryList Blocked
   3. 当thread-0退出同步块解锁时，使用cas将Mark Word的值恢复给对象头，失败。这时会进入重量级锁解锁流程，即按照Monitor地址找到Monitor对象，设置Owner为null，唤醒EntryList中的Blocked线程
3. 自旋优化：
   1. 访问同步块，获取monitor失败时，允许自旋重试（多核CPU有效）
   2. Java6之后自旋锁是自适应的，对象短时间内自旋操作成功过，那么认为这次自旋成功的可能性会高，就多自旋几次，反之，就少自旋甚至不自旋
   3. 自旋会占用CPU时间，单核CPU自旋就是浪费，多核CPU自旋才能发挥优势
   4. Java7之后不能控制是否开启自旋功能
4. 偏向锁：
   1. 只有第一次使用cas将线程ID设置到对象的Mark Word头，之后发现这个线程ID是自己的就表示没有竞争，不用重新cas。以后只要不发生竞争，这个对象就归该线程所有
   2. 对象创建后，偏向锁（默认开启），只有第一次用到hashcode时会赋值，撤销偏向锁。因为偏向锁存线程id占用了hashcode的存储位置
   3. 当有其他线程使用偏向锁对象时，会将偏向锁升级为轻量级锁
   4. 批量重偏向
   5. 批量撤销：当撤销偏向锁阈值超过40次之后，jvm会将整个类的所有对象变为不可偏向，新建的对象也是不可偏向的
   6. 锁消除

### B-002：Wait/Notify

#### **原理：**

1. Owner线程发现条件不满足，调用wait方法，即可进入WaitSet变为Waiting状态
2. Blocked和Waiting的线程都处于阻塞状态，不占用CPU时间片
3. Blocked线程会在Owner线程释放锁时唤醒
4. Waiting线程会在Owner线程调用Notify或NotifyAll时唤醒，但唤醒后并不意味着立刻获得锁，仍需进入EntryList重新竞争

#### **使用：**

1. sleep(long n)与wait(long n)的区别

   1. sleep是Thread的方法，而wait是Object的方法
   2. sleep不需要强制和synchronized配合使用，但wait需要和synchronized一起使用
   3. sleep在睡眠的同时，不会释放对象锁，但wait在等待的时候会释放对象锁
   4. 状态都是timed_waiting（无参数的wait是waiting）

2. 常用写法

   1. ```java
      synchronized(lock){
          while(条件不成立){
              lock.wait();
          }
      }
      
      synchronized(lock){
          lock.notifyAll();
      }
      ```

### B-003：同步模式

#### **保护性暂停**

1. 定义：
   1. 有一个结果需要从一个线程传递到另一个线程，让他们关联同一个GuardedObject
   2. 如果有结果不断从一个线程到另一个线程那么可是使用消息队列（剑生产者/消费者）
   3. JDK中，join的实现、Future的实现
2. 本质：引入第三者
3. 对比join：优于join
4. 04050
5. 













