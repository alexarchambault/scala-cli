package scala.build.tests

import com.eed3si9n.expecty.Expecty.assert as expect

import scala.build.{Bloop, RepositoryUtils}

class BloopTests extends TestUtil.ScalaCliBuildSuite {

  test("Bloop extra repositories are empty for stable versions") {
    val repositories = Bloop.extraRepositoriesForBloop("2.0.15", "3.3.5")
    expect(repositories.isEmpty)
  }

  test("Bloop extra repositories include snapshots for snapshot Bloop versions") {
    val repositories = Bloop.extraRepositoriesForBloop("2.0.6-51-38c118d4-SNAPSHOT", "3.3.5")
    expect(repositories.contains(coursier.Repositories.centralMavenSnapshots))
    expect(repositories.contains(RepositoryUtils.snapshotsRepository))
    expect(!repositories.contains(RepositoryUtils.scala3NightlyRepository))
  }

  test("Bloop extra repositories include Scala nightly only for nightly Scala") {
    val repositories = Bloop.extraRepositoriesForBloop("2.0.15", "3.7.0-RC1-bin-20250520-2f95f3f-NIGHTLY")
    expect(repositories.contains(RepositoryUtils.scala3NightlyRepository))
    expect(!repositories.contains(coursier.Repositories.centralMavenSnapshots))
    expect(!repositories.contains(RepositoryUtils.snapshotsRepository))
  }
}
