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

# A-001：SpringApplication

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

### C-001：SpringApplication创建

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

### C-002：SpringApplication#run

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

#### D-001：事件发布器组合

`SpringApplicationRunListeners listeners = getRunListeners(args);`

通过类路径发现并实例化所有SpringApplicationRunListener，实现应用启动生命周期钩子统一入口，用来在SpringApplication.run的关键阶段发布事件、记录指标和做早起初始化

```java
private SpringApplicationRunListeners getRunListeners(String[] args) {
    Class<?>[] types = new Class<?>[] { SpringApplication.class, String[].class };
    return new SpringApplicationRunListeners(logger,
                                             getSpringFactoriesInstances(SpringApplicationRunListener.class, types, this, args),
                                             this.applicationStartup);
}
```

1. 分析：
   1. 创建事件发布器组合SpringApplicationRunListeners，默认只有一个事件发布器EventPublishingRunListener，通过SPI机制加载
   2. EventPublishingRunListener构造：创建临时事件广播器SimpleApplicationEventMulticaster，由事件发布器持有，同时注册SpringApplication构造SPI机制读取到的ApplicationListener进临时事件广播器
2. 设计原由：
   1. 在容器启动的超早期（无ApplicationContext）也有事件与扩展点
   2. 是容器启动具有可观测性
3. 扩展点：
   1. 扩展SpringApplicationRunListener与ApplicationListener接入自定义度量、日志、配置预处理、健康探针

#### D-002：配置环境

`ConfigurableEnvironment environment = prepareEnvironment(listeners, bootstrapContext, applicationArguments);`

在ApplicationContext创建之前，把未来整个容器要遵循的外部化配置模型（属性源、Profile、配置文件、日志配置、绑定规则等）统一收敛到一个ConfigurableEnvironment，并通过事件机制把它开放给所有扩展点。

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

#### D-003：创建容器

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

#### D-004：准备容器

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

1. context.setEnvironment(environment)：把环境配置绑定到容器
   1. 后续操作基于同一套环境
2. postProcessApplicationContext(context)：容器级基础能力注入
   1. 注入BeanNameGenerator：影响组件扫描/注册beanName生成策略
   2. 注入ResourceLoader/ClassLoader：影响资源解析、类加载隔离策略
   3. 注入ConversionService：影响属性绑定与类型转换一致性
3. applyInitializers(context)：执行用户/框架扩展点：ApplicationContextInitializer
   1. 依次执行SPI机制加载到的ApplicationContextInitializer，在BeanDefinition加载前对context做结构性调整（注册property sources、设置parent、注册额外的后置处理器等）
   2. 这里对泛型required做了校验（保证initializer只作用于匹配的context类型）：扩展点安全护栏
4. listeners.contextPrepared(context)：发布启动生命周期回调
   1. 扩展点：参照C-001
5. bootstrapContext.close(context)：关闭BootstrapContext：早期容器退场
   1. 前置阶段切换到正式ApplicationContext阶段，并把必要的内容衔接到主容器
6. logStartupInfo：启动信息日志
7. beanFactory.registerSingleton()：注册Boot级别单例
   1. 把启动参数、banner等作为容器内可注入对象提供给业务与框架组件
8. 治理BeanFactory行为：系统治理策略开关
   1. setAllowCircularReferences(this.allowCircularReferences);
      1. 是否允许循环依赖：影响架构健康度与调试复杂度
   2. setAllowBeanDefinitionOverriding(this.allowBeanDefinitionOverriding)：
      1. 是否允许覆盖BeanDefinition：影响模块边界与可预测性
9. 注册BeanFactoryPostProcessor
   1. LazyInitializationBeanFactoryPostProcessor、PropertySourceOrderingBeanFactoryPostProcessor
      1. 核心扩展点：通过后置处理器在refresh过程中“改写/增强容器行为”
10. load(context, sources.toArray(new Object[0]))：加载sources，形成BeanDefinitions
    1. **参照A-002：BeanDefinitionLoader**
11. contextLoaded(context)：发布启动生命周期回调
    1. BeanDefinitions已加载完成，但还没refresh

**（三）设计模式**

1. Template Method（模板方法） run() 固定启动骨架，而 prepareContext/postProcessApplicationContext/applyInitializers/load 等是可覆写的步骤与钩子，保证“主流程稳定 + 局部可定制”。
2. Strategy（策略）/ 可插拔扩展点

3. Initializers、Listeners、BeanDefinitionLoader 都是策略化组件，来源通常是 spring.factories 或显式设置，形成“平台能力插件化”。
4. Observer（观察者） contextPrepared/contextLoaded 将关键里程碑事件广播出去，让日志、配置、诊断、AOT、Web 等模块不用相互依赖。
5. Inversion of Control 的“组合根（Composition Root）”思想 prepareContext 集中完成“把系统的组成部分装配进容器”的工作，把复杂性收敛到一个阶段，降低后续生命周期的不确定性。
6. Governance by Configuration（治理即配置） 循环依赖、覆盖策略、懒加载等，都是架构治理开关，体现平台对应用的“约束与护栏”。

**（四）架构**

1. 把启动分成“准备（environment/extension）→ 装配（prepareContext）→ 执行（refresh）”三段，能显著提升可维护性与可扩展性。
2. 真正的扩展点不要插在 refresh 之后；要插在 refresh 之前 （initializer、BFPP、listeners 的 contextPrepared/contextLoaded），才能影响容器的结构与行为。
3. 平台化能力最好以“容器内单例 + 后置处理器 + 生命周期事件”三件套交付，而不是散落的静态工具类。

#### D-005：刷新容器

`refreshContext(context);`

（一）分析

1. **参照A-003：AnnotationConfigServletWebServerApplicationContext**

# A-002：BeanDefinitionLoader

## B-001：前言

Bean定义加载

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

### C-001：SpringApplication入口

```java
protected void load(ApplicationContext context, Object[] sources) {
    if (logger.isDebugEnabled()) {
        logger.debug("Loading source " + StringUtils.arrayToCommaDelimitedString(sources));
    }
    BeanDefinitionLoader loader = createBeanDefinitionLoader(getBeanDefinitionRegistry(context), sources);
    if (this.beanNameGenerator != null) {
        loader.setBeanNameGenerator(this.beanNameGenerator);
    }
    if (this.resourceLoader != null) {
        loader.setResourceLoader(this.resourceLoader);
    }
    if (this.environment != null) {
        loader.setEnvironment(this.environment);
    }
    loader.load();
}
```

#### D-001：BeanDefinitionRegistry

`getBeanDefinitionRegistry(context)`

ApplicationContext是外观，BeanDefinitionRegistry才是“定义层”的核心写入口

```java
private BeanDefinitionRegistry getBeanDefinitionRegistry(ApplicationContext context) {
    if (context instanceof BeanDefinitionRegistry) {
        return (BeanDefinitionRegistry) context;
    }
    if (context instanceof AbstractApplicationContext) {
        return (BeanDefinitionRegistry) ((AbstractApplicationContext) context).getBeanFactory();
    }
    throw new IllegalStateException("Could not locate BeanDefinitionRegistry");
}
```

#### D-002：BeanDefinitionReader

`createBeanDefinitionLoader(registry, sources)`

```java
BeanDefinitionLoader(BeanDefinitionRegistry registry, Object... sources) {
    Assert.notNull(registry, "Registry must not be null");
    Assert.notEmpty(sources, "Sources must not be empty");
    this.sources = sources;
    this.annotatedReader = new AnnotatedBeanDefinitionReader(registry);
    this.xmlReader = (XML_ENABLED ? new XmlBeanDefinitionReader(registry) : null);
    this.groovyReader = (isGroovyPresent() ? new GroovyBeanDefinitionReader(registry) : null);
    this.scanner = new ClassPathBeanDefinitionScanner(registry);
    this.scanner.addExcludeFilter(new ClassExcludeFilter(sources));
}
```

#### D-003：load

```java
private void load(Object source) {
    Assert.notNull(source, "Source must not be null");
    if (source instanceof Class<?>) {
        load((Class<?>) source);
        return;
    }
    if (source instanceof Resource) {
        load((Resource) source);
        return;
    }
    if (source instanceof Package) {
        load((Package) source);
        return;
    }
    if (source instanceof CharSequence) {
        load((CharSequence) source);
        return;
    }
    throw new IllegalArgumentException("Invalid source type " + source.getClass());
}
```

**（一）分析**

1. Class<?>
   1. load(Class)
   2. 处理逻辑：
      - 若是 GroovyBeanDefinitionSource（Boot 的 DSL 扩展点），先让 groovy reader 收集 beans
      - isEligible 过滤匿名类/groovy closure/无构造器的类型（见 isEligible ）
      - 然后 annotatedReader.register(source) ：把这个类作为一个 bean definition 注册进 registry
   3. 架构含义：
      - Boot 默认把“主启动类（@SpringBootApplication）”作为最核心 source
      - 后续真正的组件扫描、自动配置导入等，往往是在 refresh 期间由配置类解析链条完成，而不是这里显式扫描全部包

2. Resource（通常是 XML / Groovy 文件）
   1. load(Resource)
   2. 通过文件扩展名分派：
      - .groovy → groovyReader.loadBeanDefinitions
      - 其他 → xmlReader.loadBeanDefinitions
   3. 关键架构点：
      - XML 支持可以通过 spring.xml.ignore 彻底禁用（见 XML_ENABLED ），利于精简运行时与 AOT 场景
3. Package
   1. load(Package)
   2. scanner.scan(packageName) ：走组件扫描，把候选组件变成 bean definitions 注册进 registry
4. CharSequence（字符串 source）
   1. load(CharSequence)
   2. 这是最“框架友好”的一类输入，因为它允许：
      - 先 resolvePlaceholders （依赖 environment）
      - 尝试当作 class 全限定名加载
      - 不行则按资源模式去找（ResourcePatternResolver 支持 classpath*: 这类模式）
      - 再不行就尝试当作 package（还有一段“探测包里任意 class”来确认包存在的逻辑）
   3. 架构意义：
      - 字符串 source 是一种“松耦合输入协议”，适合外部化配置或不同启动器拼装 sources

**（二）设计模式**

1. Facade（门面模式）
   1. BeanDefinitionLoader 对外只暴露 setXxx + load() ，内部组合 annotated/xml/groovy/scanner

   2. 价值：上层 SpringApplication 不需要知道“到底是 XML 还是注解还是扫描”，统一入口降低复杂度
2. Strategy（策略模式）
   1. “如何读取定义”由不同 reader/scanner 策略承担
   2. “source 类型 → 加载策略”是一个运行时分派策略（Class/Resource/Package/CharSequence）
3. Template Method / Hook（模板方法/钩子）
   1. SpringApplication.createBeanDefinitionLoader(...) 是可覆写的工厂方法（钩子）
   2. 允许高级定制：替换 loader、替换 scanner 策略、加额外过滤器等，而不改主流程
4. Separation of Concerns（关注点分离）
   1. SpringApplication 负责启动编排（orchestration）
   2. BeanDefinitionLoader 负责定义加载（definition loading）
   3. refresh 负责生命周期执行（execution）
   4. 这是大型框架可演进的核心结构

**（三）架构**

1. 一致性注入（Naming/Resource/Environment）
   1. SpringApplication.load 把 beanNameGenerator/resourceLoader/environment 下发给 loader

   2. BeanDefinitionLoader 再把它们分发给 annotatedReader、scanner、xmlReader（见 setBeanNameGenerator 、 setResourceLoader 、 setEnvironment ）
   3. 架构结论：Boot 的启动装配强调“同一套策略贯穿所有加载通道”，避免出现“扫描用一套命名，XML 用另一套命名”的割裂
2. 扫描去重：ClassExcludeFilter
   1. 构造器里 scanner.addExcludeFilter(new ClassExcludeFilter(sources)) （见 BeanDefinitionLoader. ）
   2. 它的目的很明确：如果 sources 里已经显式给了某些 Class，就不要在包扫描时又把它们当成候选组件再注册一次（见 ClassExcludeFilter ）
   3. 架构结论：这是“显式注册优先，扫描补充”的策略，减少重复定义风险

**（四）总结**

1. 这里只负责把sources这一批入口配置对应的BeanDefinition注册进BeanDefinitionRegistry
2. 绝大多数BeanDefinitionRegistry在refresh()过程中“二次/递归”产生



# A-003：ApplicationContext

## B-001：前言

**详细分析：AnnotationConfigServletWebServerApplicationContext**

**（一）定位**

1. Servlet Web 应用的直接是应用上下文+内嵌容器启动器
   1. 支持@Configuration/@Component、classpath扫描，把类转换成BeanDefinition
   2. Servlet WebServer继承自ServletWebServerApplicationContext，在refresh生命周期里创建并启动内嵌WebServer（Tomcat/Jetty/Undertow）

**（二）核心职责拆解**

1. AnnotatedBeanDefinitionReader
2. ClassPathBeanDefinitionScanner

## B-002：调试

```java
@SpringBootApplication
public class B01Application {
    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(B01Application.class, args);
    }
}
```

## B-003：源码分析

### C-001：AnnotationConfigServletWebServerApplicationContext构造

```java
public AnnotationConfigServletWebServerApplicationContext(DefaultListableBeanFactory beanFactory) {
    super(beanFactory);
    this.reader = new AnnotatedBeanDefinitionReader(this);
    this.scanner = new ClassPathBeanDefinitionScanner(this);
}
```

### C-002：AbstractApplicationContext#refresh

`AbstractApplicationContext`

```java
public void refresh() throws BeansException, IllegalStateException {
    synchronized (this.startupShutdownMonitor) {
        StartupStep contextRefresh = this.applicationStartup.start("spring.context.refresh");
        prepareRefresh();
        ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();
        prepareBeanFactory(beanFactory);
        try {
            postProcessBeanFactory(beanFactory);
            StartupStep beanPostProcess = this.applicationStartup.start("spring.context.beans.post-process");
            invokeBeanFactoryPostProcessors(beanFactory);
            registerBeanPostProcessors(beanFactory);
            beanPostProcess.end();
            initMessageSource();
            initApplicationEventMulticaster();
            onRefresh();
            registerListeners();
            finishBeanFactoryInitialization(beanFactory);
            finishRefresh();
        }

        catch (BeansException ex) {
            if (logger.isWarnEnabled()) {
                logger.warn("Exception encountered during context initialization - " +
                            "cancelling refresh attempt: " + ex);
            }
            destroyBeans();
            cancelRefresh(ex);
            throw ex;
        }

        finally {
            resetCommonCaches();
            contextRefresh.end();
        }
    }
}
```



# A-004：AnnotatedBeanDefinitionReader

## B-001：前言

## B-002：调试

## B-003：源码解析

### C-001：AnnotatedBeanDefinitionReader构造

```java
public AnnotatedBeanDefinitionReader(BeanDefinitionRegistry registry, Environment environment) {
    Assert.notNull(registry, "BeanDefinitionRegistry must not be null");
    Assert.notNull(environment, "Environment must not be null");
    this.registry = registry;
    this.conditionEvaluator = new ConditionEvaluator(registry, environment, null);
    AnnotationConfigUtils.registerAnnotationConfigProcessors(this.registry);
}
```

#### D-001：ConditionEvaluator

`this.conditionEvaluator = new ConditionEvaluator(registry, environment, null);`

条件评估器

TODO：@Profile/@Conditional；ConfigurationClassParser、ConfigurationClassBeanDefinitionReader

**（一）定位**

1. BeanDefinition真正进入registry之前做一次装配期准入校验

**（二）分析**

```java
// 注意入参
public ConditionEvaluator(@Nullable BeanDefinitionRegistry registry,
                          @Nullable Environment environment, @Nullable ResourceLoader resourceLoader) {
    this.context = new ConditionContextImpl(registry, environment, resourceLoader);
}
// 同一个条件体系可以在不同宿主下运行（ApplicationContext / 纯 BeanFactory / 扫描器场景），靠“能力探测 + 默认实现”保证可用性与一致性。
public ConditionContextImpl(@Nullable BeanDefinitionRegistry registry,
                            @Nullable Environment environment, @Nullable ResourceLoader resourceLoader) {

    this.registry = registry;
    this.beanFactory = deduceBeanFactory(registry);
    this.environment = (environment != null ? environment : deduceEnvironment(registry));
    this.resourceLoader = (resourceLoader != null ? resourceLoader : deduceResourceLoader(registry));
    this.classLoader = deduceClassLoader(resourceLoader, this.beanFactory);
}
```

**（三）架构**

1. 把环境差异收敛到装配期 ：条件系统把“是否注册”变成确定性决策，减少运行期分支与配置污染。
2. 策略 + 能力探测 ：同一套规则引擎在不同容器宿主下可运行，靠推导/降级保证一致性。
3. 分阶段条件 ：通过 ConfigurationCondition 把“解析配置类”和“注册普通 bean”区分开，解决复杂装配中的时序问题。

#### D-002：registerAnnotationConfigProcessors();

`AnnotationConfigUtils.registerAnnotationConfigProcessors(this.registry);`

**（一）定位**

Spring 的核心运行时并不“天然理解” @Configuration/@Bean/@Autowired/@Resource/@EventListener... 。 它通过注册一组 BeanFactoryPostProcessor / BeanPostProcessor / 辅助策略对象 来让容器获得这些能力。

registerAnnotationConfigProcessors 做的就是：

1. 给 BeanFactory 设置两个关键策略（排序 + 候选解析）
2. 向 registry 注册一组“内部基础设施 bean definition”（role=INFRASTRUCTURE）
3. 具备幂等特征：如果已经注册过同名定义，就跳过（ containsBeanDefinition 检查 ）

**（二）分析**

```java
public static Set<BeanDefinitionHolder> registerAnnotationConfigProcessors(
    BeanDefinitionRegistry registry, @Nullable Object source) {
    // 尝试解包成DefaultListableBeanFactory
    DefaultListableBeanFactory beanFactory = unwrapDefaultListableBeanFactory(registry);
    if (beanFactory != null) {
        if (!(beanFactory.getDependencyComparator() instanceof AnnotationAwareOrderComparator)) {
            // 让容器在解析依赖/排序时理解@Order、Ordered等注解/接口的优先级规则
            // 架构：全局排序语义的注入
            beanFactory.setDependencyComparator(AnnotationAwareOrderComparator.INSTANCE);
        }
        if (!(beanFactory.getAutowireCandidateResolver() instanceof ContextAnnotationAutowireCandidateResolver)) {
            // 增强“候选 Bean 选择”能力，支持更多基于注解的注入判定（典型如 @Lazy 注入代理、 @Qualifier 族的匹配等）。
            // 架构：这是把“依赖解析规则”从硬编码升级为可替换策略，是 Spring 可扩展性的关键支点之一
            beanFactory.setAutowireCandidateResolver(new ContextAnnotationAutowireCandidateResolver());
        }
    }
    // 注册 5 类关键基础设施处理器（真正让注解生效）
    // 它注册的 bean 都会被标记为 ROLE_INFRASTRUCTURE （ registerPostProcessor ），并使用一组固定的内部 beanName 常量（如 internalAutowiredAnnotationProcessor 等）。
    Set<BeanDefinitionHolder> beanDefs = new LinkedHashSet<>(8);
    // 以下详细分析：后文
    if (!registry.containsBeanDefinition(CONFIGURATION_ANNOTATION_PROCESSOR_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition(ConfigurationClassPostProcessor.class);
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, CONFIGURATION_ANNOTATION_PROCESSOR_BEAN_NAME));
    }

    if (!registry.containsBeanDefinition(AUTOWIRED_ANNOTATION_PROCESSOR_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition(AutowiredAnnotationBeanPostProcessor.class);
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, AUTOWIRED_ANNOTATION_PROCESSOR_BEAN_NAME));
    }

    // Check for JSR-250 support, and if present add the CommonAnnotationBeanPostProcessor.
    if (jsr250Present && !registry.containsBeanDefinition(COMMON_ANNOTATION_PROCESSOR_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition(CommonAnnotationBeanPostProcessor.class);
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, COMMON_ANNOTATION_PROCESSOR_BEAN_NAME));
    }

    // Check for JPA support, and if present add the PersistenceAnnotationBeanPostProcessor.
    if (jpaPresent && !registry.containsBeanDefinition(PERSISTENCE_ANNOTATION_PROCESSOR_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition();
        try {
            def.setBeanClass(ClassUtils.forName(PERSISTENCE_ANNOTATION_PROCESSOR_CLASS_NAME,
                                                AnnotationConfigUtils.class.getClassLoader()));
        }
        catch (ClassNotFoundException ex) {
            throw new IllegalStateException(
                "Cannot load optional framework class: " + PERSISTENCE_ANNOTATION_PROCESSOR_CLASS_NAME, ex);
        }
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, PERSISTENCE_ANNOTATION_PROCESSOR_BEAN_NAME));
    }

    if (!registry.containsBeanDefinition(EVENT_LISTENER_PROCESSOR_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition(EventListenerMethodProcessor.class);
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, EVENT_LISTENER_PROCESSOR_BEAN_NAME));
    }

    if (!registry.containsBeanDefinition(EVENT_LISTENER_FACTORY_BEAN_NAME)) {
        RootBeanDefinition def = new RootBeanDefinition(DefaultEventListenerFactory.class);
        def.setSource(source);
        beanDefs.add(registerPostProcessor(registry, def, EVENT_LISTENER_FACTORY_BEAN_NAME));
    }

    return beanDefs;
}
```

**ConfigurationClassPostProcessor（最关键的“配置类引擎”）**

注册点： L163-L167 beanName 常量： CONFIGURATION_ANNOTATION_PROCESSOR_BEAN_NAME 类本身定位（它是 RegistryPostProcessor 且 PriorityOrdered）： ConfigurationClassPostProcessor.java

它的职责（架构级总结）：

1. 解析 @Configuration 、 @ComponentScan 、 @Import 、 @ImportResource 等
2. 把 @Bean 方法“翻译”为真正的 BeanDefinition 注册到容器
3. 因为它是 BeanDefinitionRegistryPostProcessor + PriorityOrdered ，所以会在 refresh 早期优先执行，确保其他后处理器运行前，配置类已经展开成完整的 bean 定义集合
  如果你只记住一个： Spring Java Config 的核心不是“扫描到类”，而是这个处理器把配置模型编译进 BeanDefinition 图。

**AutowiredAnnotationBeanPostProcessor（字段/构造器/方法注入引擎）**

注册点： L169-L173 beanName 常量： AUTOWIRED_ANNOTATION_PROCESSOR_BEAN_NAME

职责：

1. 处理 @Autowired 、 @Value ，以及（通常）兼容 javax.inject.Inject
2. 发生在 Bean 实例化之后、属性填充阶段（BPP 生命周期）
  架构视角：它把“依赖注入”从 XML/property 映射，升级为“基于注解的依赖声明”。

**CommonAnnotationBeanPostProcessor（JSR-250：@Resource 等）**

注册点： L175-L180 beanName 常量： COMMON_ANNOTATION_PROCESSOR_BEAN_NAME

它是“可选注册”：只要类路径存在 javax.annotation.Resource 才启用（ jsr250Present 检测 ）。

职责常见包括：

1. @Resource 按名称/类型注入
2. @PostConstruct/@PreDestroy 生命周期回调（取决于 Spring 版本/组合的处理器，整体上 JSR-250 支持在这里被引入）
  架构视角：这是典型的 “可选能力装配” ：根据 classpath 自动启用模块，减少硬依赖。

**PersistenceAnnotationBeanPostProcessor（JPA：@PersistenceContext）**

注册点： L182-L195 beanName 常量： PERSISTENCE_ANNOTATION_PROCESSOR_BEAN_NAME

同样是“可选注册”：需要存在 javax.persistence.EntityManagerFactory 且能加载 PersistenceAnnotationBeanPostProcessor （ jpaPresent 检测 ）。

架构视角：Spring 在这里实现了对 ORM/持久化栈的“软集成”，避免 core context 强绑定 JPA。

**@EventListener 体系：EventListenerMethodProcessor + DefaultEventListenerFactory**

注册点：

1. EVENT_LISTENER_PROCESSOR_BEAN_NAME
2. EVENT_LISTENER_FACTORY_BEAN_NAME
  职责：

3. EventListenerMethodProcessor 扫描 bean 上的 @EventListener 方法并注册为监听器
4. DefaultEventListenerFactory 用于创建对应的监听器适配器实例
  架构视角：这是把“事件驱动编程模型”编译进容器的一套处理链，属于典型的“声明式监听器”。

**D-003：AnnotationBeanNameGenerator**

TODO：FullyQualifiedAnnotationBeanNameGenerator



