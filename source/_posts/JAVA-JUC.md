---
title: JAVA-JUC
date: 2025-12-14 15:23:48
description: JAVA-JUC 并发
tags:
    - JAVA
    - JUC
categories:
    - JAVA
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

#### 轻量级锁：

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

   

   

   

