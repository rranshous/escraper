defmodule Escraper.WorkQueue do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :workqueue }, __MODULE__, [], [])
  end

  def init(queue) do
    { :ok, HashSet.new(queue) }
  end

  def handle_cast({ :add, page }, queue) do
    IO.puts "work queue adding: #{page.url}"
    :gen_server.cast(:pagescraper, :work_added)
    { :noreply, HashSet.put(queue, page) }
  end

  def handle_call(:pop, _from, queue) do
    { h, queue } = pop_hashset(queue)
    { :reply, h, queue }
  end

  def pop_hashset(set) do
    h = Enum.at(set, 0)
    { h, HashSet.delete(set, h) }
  end
end
