---
title: Spring
date: 2025-12-18 20:07:05
description: Spring 框架
tags:
  - JAVA
  - Spring
categories:
  - JAVA
top_img: false
cover: /image/post_cover/spring-framework-min.svg
---

## A-00X：SpringApplication

**流程分析**

1. SpringApplication构造
   1. 保存启动类信息
   2. 初始化环境
   3. 加载*spring.factories*设置*ApplicationContextInitializer*
   4. 加载*spring.factories*设置监听器
   5. 设置启动类信息
2. SpringApplication#run
   1. 加载 *spring.factories* 获取Spring监听器
   2. 构造容器环境
   3. 创建上下文对象
   4. 准备刷新上下文
   5. 刷新上下文
   6. 结束刷新
   7. 容器成功创建

## A-00X：Spring-Event

### B-001：分析

1. 要素：事件源，事件，事件发布器，事件广播器，事件监听器

#### **事件广播器**

1. 定位： *Spring Event* 中心
2. 前提条件： *BeanDefinition load finish*
3. 初始化时机： *ApplicationContext#refresh->#initApplicationEventMulticaster*

#### **事件监听器**

1. 分类：Bean类型监听器，非Bean类型监听器
2. 注册 *ApplicationListener* 到 *ApplicationEventMulticaster*
   1. 前提条件：*ApplicationEventMulticaster init finish*
   2. 执行时机：*ApplicationContext#refresh->#registerListeners*
   3. 初始化非Bean类型监听器至 *ApplicationEventMulticaster*
   4. 初始化Bean类型监听器至 *ApplicationEventMulticaster*
   5. *ApplicationListenerDetector* 保证特殊 *ApplicationListener* 正确注册进 *ApplicationEventMulticaster*
3. 非Bean类型监听器
   1. *AbstractApplicationContext.applicationListeners*
   2. 类型
      1. 容器内置的核心监听器：Spring容器自身会在初始化时，手动实例化并添加一批内置监听器，用于处理容器生命周期事件，无需BeanFactory管理：属于容器级别，作为容器启动的基础组件
      2. 开发者手动调用API添加的监视器
      3. SPI加载的监听器：扩展点：示例：Spring Boot *META-INF/spring.factories*，加载时机 *SpringApplication#constructor*
4. Bean类型监听器
   1. 实现方式：接口、注解
5. ApplicationListenerDetector
   1. Spring中有些Bean的创建/初始化时机可能晚于 *ApplicationContext#refresh->#registerListeners* 
   2. 具体场景（错过核心自动注册流程）
      1. 懒加载单例Bean：实例化延迟到「第一次 `getBean()`」时，而 `registerListeners()` 阶段仅注册了「Bean 名称」到广播器，若事件触发时 Bean 尚未实例化，可能出现「监听器未生效」（或实例化后未被广播器感知）。
      2. 原型Bean：`registerListeners()` 阶段仅注册「原型 Bean 的名称」到广播器，事件广播器每次触发事件时会 `getBean()` 获取新实例
         1. 若原型 Bean 未被 `getBean()` 过，事件触发时才生成实例，可能错过早期事件；
         2. 若开发者手动 `getBean()` 生成原型实例，该实例不会被核心流程识别，无法注册到广播器。
      3. BeanFactoryPostProcessor 动态生成Bean
         1. 在 `refresh()` 的 `invokeBeanFactoryPostProcessors()` 阶段（早于 `registerListeners()`），用于修改 / 新增 `BeanDefinition`；
         2. 若 BFPP 动态生成 `ApplicationListener` 的 `BeanDefinition` 时存在「延迟 / 不完整」（比如 BFPP 执行顺序靠后），`registerListeners()` 阶段的 `getBeanNamesForType()` 扫描可能漏扫该 BeanDefinition，导致核心流程完全无法识别。
      4. 手动创建的Bean
      5. 后置添加的Bean
   3. 本质上：Spring 核心流程是「提前扫描 BeanDefinition + 延迟实例化」的被动模式，而 `ApplicationListenerDetector` 是「实例化后主动检测 + 即时注册」的主动模式，覆盖所有「Bean 实例化时机脱离核心流程窗口期」的场景，确保所有 `ApplicationListener` 类型的 Bean 都能被正确注册。

#### **事件广播器-事件监听器**

1. 执行时机：*context.publishEvent*
2. 获取监听器
   1. 解析事件类型
   2. 获取所有候选监听器（合并两类监听器）
      1. Bean类型监听器如果没被实例化：仅在首次触发匹配事件时才通过 `beanFactory.getBean()` 获取实例；（TODO）
      2. 懒加载单例 Bean / 原型 Bean：此时才会触发实例化（原型 Bean 每次获取都是新实例）；
      3. 若 Bean 已被销毁 / 移除，会捕获 `NoSuchBeanDefinitionException` 并忽略。
   3. 筛选匹配当前时间的监听器
   4. 对监听器排序
   5. 缓存匹配结果（原型Bean不缓存，每次获取新实例）
