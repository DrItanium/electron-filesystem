package clips;

public class InstanceAddressValue extends InstanceValue
{
	private Environment owner;

	/*************************/
	/* InstanceAddressValue: */
	/*************************/
	public InstanceAddressValue(
			long value,
			Environment env)
	{
		super(new Long(value));

		owner = env;
	}

	/*******************/
	/* getEnvironment: */
	/*******************/
	public Environment getEnvironment()
	{ return owner; }

	/***********************/
	/* getInstanceAddress: */
	/***********************/     
	public long getInstanceAddress()
	{ return ((Long) getValue()).longValue(); }

	/******************/
	/* directGetSlot: */
	/******************/     
	public PrimitiveValue directGetSlot(
			String slotName)
	{ return Environment.directGetSlot(this,slotName); }

	/********************/
	/* getInstanceName: */
	/********************/     
	public String getInstanceName()
	{ return Environment.getInstanceName(this); }

	/*************/
	/* toString: */
	/*************/
	public String toString()
	{        
		return "<Instance-" + getInstanceName() + ">";
	}

	/***********/
	/* retain: */
	/***********/
	public void retain()
	{
		//System.out.println("InstanceAddressValue retain");
		owner.incrementInstanceCount(this);
	}

	/************/
	/* release: */
	/************/
	public void release()
	{
		//System.out.println("InstanceAddressValue release");
		owner.decrementInstanceCount(this);
	}
}
