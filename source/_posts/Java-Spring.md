---
title: Java-Spring
date: 2025-12-18 20:07:05
description: Java-Spring 框架
tags:
  - Java
  - Spring
categories:
  - Java
top_img: false
cover: /image/post_cover/spring-framework-min.svg
---

**SpringBoot：2.7.18**

**文档版本：GIT**：迭代

# A-00X：SpringApplication

## B-001：前言

SpringBoot执行流程

## B-002：调试

```java
@SpringBootApplication
public class B01Application {
    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(B01Application.class, args);
    }
}
```

## B-003：源码解析

### SpringApplication创建

```java
public SpringApplication(ResourceLoader resourceLoader, Class<?>... primarySources) {
    this.resourceLoader = resourceLoader;
    Assert.notNull(primarySources, "PrimarySources must not be null");
    // 获取源：主源
    this.primarySources = new LinkedHashSet<>(Arrays.asList(primarySources));
    // 推断应用程序类型：根据类路径下是否存在某些特殊类来判断
    this.webApplicationType = WebApplicationType.deduceFromClasspath();
    // SPI机制加载：BootstrapRegistryInitializer（引导上下文扩展）
    this.bootstrapRegistryInitializers = new ArrayList<>(
        getSpringFactoriesInstances(BootstrapRegistryInitializer.class));
    // SPI机制加载：ApplicationContextInitializer（应用主上下文扩展）
    setInitializers((Collection) getSpringFactoriesInstances(ApplicationContextInitializer.class));
    // SPI机制加载：ApplicationListener（应用事件监听器）,供SpringApplication#run方法中临时事件广播器使用
    setListeners((Collection) getSpringFactoriesInstances(ApplicationListener.class));
    // 推断主类
    this.mainApplicationClass = deduceMainApplicationClass();
}
```

### SpringApplication#run

```java
public ConfigurableApplicationContext run(String... args) {
    long startTime = System.nanoTime();
    // 创建引导上下文：执行引导上下文扩展
    DefaultBootstrapContext bootstrapContext = createBootstrapContext();
    ConfigurableApplicationContext context = null;
    configureHeadlessProperty();
    // 创建：SpringApplicationRunListeners（事件发布器组合）：SPI机制加载SpringApplicationRunListener（事件发布器）
    SpringApplicationRunListeners listeners = getRunListeners(args);
    // 发布：ApplicationStartingEvent
    listeners.starting(bootstrapContext, this.mainApplicationClass);
    try {
        // 解析命令行：ApplicationArguments
        ApplicationArguments applicationArguments = new DefaultApplicationArguments(args);
        // 准备环境
        ConfigurableEnvironment environment = prepareEnvironment(listeners, bootstrapContext, applicationArguments);
        configureIgnoreBeanInfo(environment);
        Banner printedBanner = printBanner(environment);
        // 创建容器
        context = createApplicationContext();
        context.setApplicationStartup(this.applicationStartup);
        // 准备容器：调用ApplicationContextInitializer（对context增强）、加载BeanDefinition
        prepareContext(bootstrapContext, context, environment, listeners, applicationArguments, printedBanner);
        // 刷新容器：
        refreshContext(context);
        afterRefresh(context, applicationArguments);
        Duration timeTakenToStartup = Duration.ofNanos(System.nanoTime() - startTime);
        if (this.logStartupInfo) {
            new StartupInfoLogger(this.mainApplicationClass).logStarted(getApplicationLog(), timeTakenToStartup);
        }
        listeners.started(context, timeTakenToStartup);
        // 执行runner：扩展：业务可能需要：执行两类：ApplicationRunner、CommandLineRunner
        callRunners(context, applicationArguments);
    }
    catch (Throwable ex) {
        handleRunFailure(context, ex, listeners);
        throw new IllegalStateException(ex);
    }
    try {
        Duration timeTakenToReady = Duration.ofNanos(System.nanoTime() - startTime);
        listeners.ready(context, timeTakenToReady);
    }
    catch (Throwable ex) {
        handleRunFailure(context, ex, null);
        throw new IllegalStateException(ex);
    }
    return context;
}
```

#### C-001：事件发布器组合

`SpringApplicationRunListeners listeners = getRunListeners(args);`

通过类路径发现并实例化所有SpringApplicationRunListener，实现应用启动生命周期钩子统一入口，用来在SpringApplication.run的关键阶段发布事件、记录指标和做早起初始化

1. 分析：
   1. 创建事件发布器组合SpringApplicationRunListeners，默认只有一个事件发布器EventPublishingRunListener，通过SPI机制加载
   2. EventPublishingRunListener构造：创建临时事件广播器SimpleApplicationEventMulticaster，由事件发布器持有，同时注册SpringApplication构造SPI机制读取到的ApplicationListener进临时事件广播器
2. 设计原由：
   1. 在容器启动的超早期（无ApplicationContext）也有事件与扩展点
   2. 是容器启动具有可观测性
3. 扩展点：
   1. 扩展SpringApplicationRunListener与ApplicationListener接入自定义度量、日志、配置预处理、健康探针

```java
private SpringApplicationRunListeners getRunListeners(String[] args) {
    Class<?>[] types = new Class<?>[] { SpringApplication.class, String[].class };
    return new SpringApplicationRunListeners(logger,
                                             getSpringFactoriesInstances(SpringApplicationRunListener.class, types, this, args),
                                             this.applicationStartup);
}
```



#### C-002：配置环境

`ConfigurableEnvironment environment = prepareEnvironment(listeners, bootstrapContext, applicationArguments);`

在ApplicationContext创建之前，把未来整个容器要遵循的外部化配置模型（属性源、Profile、配置文件、日志配置、绑定规则等）统一收敛到一个ConfigurableEnvironment，并通过事件机制把它开放给所有扩展点。

**（一）定位**

1. listeners.starting(....)之后：框架已进入启动状态，但还没有Environment
2. createApplicationContext()之前：容器尚未创建，任何Bean相关机制还不可用
3. printBanner(environment)之前：Banner/日志等都依赖环境中的配置
4. prepareContext(..., environment, ...) 之前：后续把 environment 注入到 context，并驱动自动配置

**（二）设计目标**

1. 在没有容器可用的情况下：完成“配置加载+扩展点调用+决策（web/非web）+影响后续启动参数”

**（三）分析**

1. **创建或获取Environment**
   1. getOrCreateEnvironment()
      1. 优先使用用户显式设置的 this.environment
      2. 否则从 applicationContextFactory 基于 webApplicationType 创建
      3. 都没有则回退到 ApplicationEnvironment
   2. Strategy+Factory：由ApplicationContextFactory决定环境/上下文类型
2. **注入基础属性源（默认属性、命令行参数）与Profile入口**
   1. configureEnvironment(environment, args)
      1. 可选设置 ApplicationConversionService
      2. configurePropertySources 把 defaultProperties、命令行参数放进 PropertySources
      3. configureProfiles （模板钩子，默认空实现）为子类预留扩展点
   2. TemplateMethod：定义基本流程，留细粒度override点
3. **把传统的PropertySource适配成Boot的配置属性模型**
   1. ConfigurationPropertySources.attach(environment)
   2. Adapter/Decorator：不改动原有的PropertySource，只是加一层解释器
4. **触发Environment Prepared：生态插件开始工作**
   1. listeners.environmentPrepared(bootstrapContext, environment);
      1. 使用事件发布器组合，发布事件，最终由EnvironmentPostProcessorApplicationListener监听事件
      2. 监听器由SPI机制加载所有EnvironmentPostProcessor，并运行
      3. ConfigDataEnvironmentPostProcessor：将 application.yml/properties 、profile 配置、 spring.config.import 等“配置数据”加载进 Environment
   2. 设计模式：
      1. Observer / Event Bus：解耦启动主干与环境加工插件
      2. Chain of Responsibility：依次调用监听器
      3. Plugin Architecture：扩展点无需改主干代码即可插入
      4. temporary：BootstrapContext：作为早期共享容器：在没有ApplicationContext时共享昂贵对象/中间态给多个post-processor
5. **调整默认属性优先级到底部**
   1. 确保默认属性兜底
   2. 架构上：越显示的配置，优先级越高
6. **把 spring.main.* 绑定回 SpringApplication（让配置反向影响启动决策）**
   1. binder 能从 application.yml 读取到 spring.main.* ，进而反向修改 SpringApplication 的行为（比如 web 类型、bannerMode、lazyInitialization、allowCircularReferences 等）
   2. Metadata-driven Bootstrap（配置驱动启动） ：启动器不是写死策略，而是被 Environment 里的元数据“参数化”。
7. **如有需要，转换 Environment 具体类型（保证与应用形态匹配）**
   1. 如果当前 environment 类型不匹配推导出的目标类型，就新建目标类型并复制
   2. Factory + Copier（对象迁移） ：避免在原对象上做“危险变形”，而是生成一个符合目标契约的新对象

**（四）总结**

在启动很早期改配置/改行为，优先级从推荐到谨慎

1. EnvironmentPostProcessor ：最适合“添加/调整 PropertySource、加载外部配置中心、注入默认值”，并可借助 Ordered 控制顺序（见接口说明： EnvironmentPostProcessor.java ）
2. ApplicationListener ：适合对环境做轻量加工，或与其他启动事件联动
3. SpringApplicationRunListener ：能力最强、侵入性也最大，更像“接管启动主干的旁路逻辑”，一般框架/基础设施组件才会用

```java
private ConfigurableEnvironment prepareEnvironment(SpringApplicationRunListeners listeners,
                                                   DefaultBootstrapContext bootstrapContext, ApplicationArguments applicationArguments) {
    // Create and configure the environment
    // 根据applicationType获取或创建ConfigurableEnvironment：SERVLET 默认ApplicationServletEnvironment
    ConfigurableEnvironment environment = getOrCreateEnvironment();
    // 讲main函数的args封装成SimpleCommandLinePropertySource加入到环境中
    // 根据SimpleCommandLinePropertySource激活响应的配置文件（TODO：这是一个空方法）
    configureEnvironment(environment, applicationArguments.getSourceArgs());
    // 将Spring支持的原生环境Environment适配成SpringBoot的ConfigurationPropertySource，让@ConfigurationProperties可以工作
    ConfigurationPropertySources.attach(environment);
    // 发布基础环境准备好事件:ApplicationEnvironmentPreparedEvent：触发EnvironmentPostProcessorApplicationListener
    // SPI机制加载EnvironmentPostProcessor
    // 执行EnvironmentPostProcessor（扩展点）自定义扩展实现方式：SPI机制
    listeners.environmentPrepared(bootstrapContext, environment);
    // SpringBoot 内置的PropertySource实现，优先级调整至最低；（不分析此方法）
    DefaultPropertiesPropertySource.moveToEnd(environment);
    Assert.state(!environment.containsProperty("spring.main.environment-prefix"),
                 "Environment prefix cannot be set via properties.");
    bindToSpringApplication(environment);
    // 判断用户是否手动设置了Environment；SpringBoot支持自定义Environment
    if (!this.isCustomEnvironment) {
        // 环境类型转换器
        EnvironmentConverter environmentConverter = new EnvironmentConverter(getClassLoader());
        // 根据应用运行场景推断环境类型，并使用环境转换器转换
        environment = environmentConverter.convertEnvironmentIfNecessary(environment, deduceEnvironmentClass());
    }
    // 上一步可能替换掉Environment实例，因此重新适配
    ConfigurationPropertySources.attach(environment);
    return environment;
}
```



#### C-003：创建容器

`context = createApplicationContext();`

创建容器壳子

**（一）定位**

决定创建哪一种ApplicationContext（Servlet/Reactive/非Web）

**（二）分析**

1. ApplicationContextFactory中环境类型与上下文类型是成对设计的
2. ApplicationContextFactory根据应用形态（SERVLET/REACTIVE/NONE）+ SPI 扩展点，选择“正确的容器实现”，并实例化它（但先不 refresh）

**（三）设计模式**

1. Strategy+Dependency Inversion：SpringApplication不依赖具体Context类，而依赖ApplicationContextFactory
2. Abstract Factory
3. SPI/Plugin Architecture

**SERVLET：AnnotationConfigServletWebServerApplicationContext**

```java
public class AnnotationConfigServletWebServerApplicationContext extends ServletWebServerApplicationContext
    implements AnnotationConfigRegistry {
    public AnnotationConfigServletWebServerApplicationContext() {
        this.reader = new AnnotatedBeanDefinitionReader(this);
        this.scanner = new ClassPathBeanDefinitionScanner(this);
    }
}
```

```java
public AnnotatedBeanDefinitionReader(BeanDefinitionRegistry registry, Environment environment) {
    Assert.notNull(registry, "BeanDefinitionRegistry must not be null");
    Assert.notNull(environment, "Environment must not be null");
    this.registry = registry;
    this.conditionEvaluator = new ConditionEvaluator(registry, environment, null);
    // SPI机制注册下面类型Bean定义
    // ConfigurationClassPostProcessor AutowiredAnnotationBeanPostProcessor
    // CommonAnnotationBeanPostProcessor
    // EventListenerMethodProcessor
    // DefaultEventListenerFactory
    AnnotationConfigUtils.registerAnnotationConfigProcessors(this.registry);
}
```

#### C-004：准备容器

`prepareContext();`

容器装配阶段：以一套可插拔方式注入参数容器，为后续的`refreshContext(context)`的重量级生命周期做准备

```java
private void prepareContext(DefaultBootstrapContext bootstrapContext, ConfigurableApplicationContext context,
                            ConfigurableEnvironment environment, SpringApplicationRunListeners listeners,
                            ApplicationArguments applicationArguments, Banner printedBanner) {
    context.setEnvironment(environment);
    postProcessApplicationContext(context);
    applyInitializers(context);
    listeners.contextPrepared(context);
    bootstrapContext.close(context);
    if (this.logStartupInfo) {
        logStartupInfo(context.getParent() == null);
        logStartupProfileInfo(context);
    }
    // Add boot specific singleton beans
    ConfigurableListableBeanFactory beanFactory = context.getBeanFactory();
    beanFactory.registerSingleton("springApplicationArguments", applicationArguments);
    if (printedBanner != null) {
        beanFactory.registerSingleton("springBootBanner", printedBanner);
    }
    if (beanFactory instanceof AbstractAutowireCapableBeanFactory) {
        ((AbstractAutowireCapableBeanFactory) beanFactory).setAllowCircularReferences(this.allowCircularReferences);
        if (beanFactory instanceof DefaultListableBeanFactory) {
            ((DefaultListableBeanFactory) beanFactory)
            .setAllowBeanDefinitionOverriding(this.allowBeanDefinitionOverriding);
        }
    }
    if (this.lazyInitialization) {
        context.addBeanFactoryPostProcessor(new LazyInitializationBeanFactoryPostProcessor());
    }
    context.addBeanFactoryPostProcessor(new PropertySourceOrderingBeanFactoryPostProcessor(context));
    // Load the sources
    Set<Object> sources = getAllSources();
    Assert.notEmpty(sources, "Sources must not be empty");
    load(context, sources.toArray(new Object[0]));
    listeners.contextLoaded(context);
}
```

**（一）定位**

1. 在refresh之前完成所有“可配置、可扩展、可治理”的注入，让refresh变成一个相对确定、可重复的过程

**（二）分析**

